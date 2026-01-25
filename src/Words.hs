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
module Words
  ( wordCount,
    wordStream,
    foldWords,
    fromUrl,
    fromFile,
  )
where

import qualified Control.Foldl as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Streaming.Char8 as B
import qualified Data.Map as Map
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
wordStream :: Monad m => Int -> B.ByteString m r -> S.Stream (S.Of Text) m ()
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

-- | Fold that counts words from a streaming bytestring
foldWords :: Monad m => B.ByteString m r -> m (Map Text Int)
foldWords s = L.purely S.fold_ wordCount (wordStream 10000 s)

-- | Run a URL stream
-- Default test URL: http://www.gutenberg.org/files/4300/4300-0.txt
fromUrl :: String -> IO (Map Text Int)
fromUrl f = do
    req <- parseRequest f
    resp <- httpBS req
    foldWords (B.fromChunks (S.each [getResponseBody resp]))

-- | Run a file stream
fromFile :: FilePath -> IO (Map Text Int)
fromFile f = runResourceT (foldWords (B.readFile f))
