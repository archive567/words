{-# LANGUAGE OverloadedStrings #-}

-- | Word counting utilities from streaming bytestrings.
--
-- Example usage:
--
-- > -- Count words from a local file
-- > result <- fromFile "other/fake.txt"
-- >
-- > -- Count words from a URL (Project Gutenberg)
-- > result <- fromUrl "http://www.gutenberg.org/files/4300/4300-0.txt"
--
-- > urlToFile "http://www.gutenberg.org/files/4300/4300-0.txt" "alice.txt"
module Words
  ( wordCount,
    wordStream,
    foldWords,
    fromUrl,
    fromUrlFreq,
    fromFile,
    streamToFile,
    urlToFile,
  )
where

import Data.ByteString.Lazy qualified as BL

import qualified Control.Foldl as L
import qualified Streaming.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Function ((&))
import Control.Category ((>>>))
import Data.Map (Map)
import Network.HTTP.Simple (httpBS, getResponseBody)
import Network.HTTP.Conduit (parseRequest)
import Control.Monad.Trans.Resource (runResourceT)

-- | Fold that counts word frequencies from a stream of Text.
wordCount :: L.Fold Text (Map Text Int)
wordCount = L.Fold (\m w -> Map.insertWith (+) w 1 m) Map.empty id

-- | Take a ByteString (a streaming library bytestring) and make a text word stream
wordStream :: Monad m => Int -> B.ByteStream m r -> S.Stream (S.Of Text) m ()
wordStream n s =
    s &
    B.words &
    B.denull &
    S.take n &
    S.mapped B.toStrict & -- the strict wall of pain
    S.map ( decodeUtf8 >>>
            Text.toLower >>>
            Text.split (not . (`elem` ['a'..'z']))) &
    S.concat &
    S.filter (/="")

streamToFile :: FilePath ->  B.ByteStream IO () -> IO () 
streamToFile f s = BL.writeFile f =<< B.toLazy_ s  

-- | Fold that counts words from a streaming bytestring
foldWords :: Monad m => B.ByteStream m r -> m (Map Text Int)
foldWords s = L.purely S.fold_ wordCount (wordStream 10000 s)

fromUrl :: String -> (B.ByteStream IO () -> IO a) -> IO a
fromUrl url f = do
    req <- parseRequest url
    resp <- httpBS req
    f (B.fromChunks (S.each [getResponseBody resp]))

-- | Run a URL stream and count word frequencies.
--
-- Example: Count words from Project Gutenberg (Alice in Wonderland):
--
-- @
-- result <- fromUrl "http://www.gutenberg.org/files/4300/4300-0.txt"
-- List.take 10 . List.sortBy (comparing (Down . snd)) . Map.toList $ result
-- -- returns: [("the",551),("and",308),("a",255),("of",247),("his",191),("he",190),("to",180),("in",170),("said",166),("i",151)]
-- @
fromUrlFreq :: String -> IO (Map Text Int)
fromUrlFreq url = fromUrl url foldWords

-- | Run a file stream
fromFile :: FilePath -> IO (Map Text Int)
fromFile f = runResourceT (foldWords (B.readFile f))

infixr 8 ⋎
(⋎) :: (a -> b -> c) -> (d -> b) -> a -> d -> c
f ⋎ g = \a b -> f a (g b)

urlToFile :: String -> FilePath -> IO ()
urlToFile = fromUrl ⋎ streamToFile

