```include
other/header.md
```

stream-words
===

Experiment in word counting using the [streaming](https://hackage.haskell.org/package/streaming-0.1.4.5/docs/Streaming.html) library.


todo
---

- [ ] https://www.reddit.com/r/haskell/comments/5x2g0r/streaming_package_vs_pipes_conduit_question_on/
- [ ] [streaming-utils](http://hackage.haskell.org/package/streaming-utils)
- [ ] Blast


[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
---

\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
\end{code}

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
---

\begin{code}
-- doctest doesn't look at the cabal file, so you need pragmas here
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
\end{code}

[libraries](https://www.stackage.org/)
---

- [protolude](https://www.stackage.org/package/protolude)
- [optparse-generic](https://www.stackage.org/package/optparse-generic)
- [streaming](https://hackage.haskell.org/package/streaming-0.1.4.5/docs/Streaming.html)
- [streaming-bytestring](http://hackage.haskell.org/package/streaming-bytestring)

\begin{code}
import Protolude
import GHC.Base (String)
import Options.Generic
import Data.Default

import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified Data.ByteString.Streaming.HTTP as BS
-- import qualified Data.ByteString.Streaming as BS
import qualified Control.Foldl as L
import qualified Data.ByteString.Streaming.Char8 as C
import Data.Text as Text
-- import Data.ByteString.Char8 as BSC
import qualified Data.Map as Map

\end{code}

code
---

- [hoogle](https://www.stackage.org/package/hoogle)

\begin{code}


data TableFormat = TableHtml | Plain deriving (Read, Show, Generic)
data Source = FileIn String | UrlIn String deriving (Read, Show, Generic)
data Destination = FileOut String | ToStdout deriving (Read, Show, Generic)
data Destinations = Destinations [(Destination, TableFormat)] deriving (Read, Show, Generic)

data Opts w =
    Opts
    { source :: w ::: Maybe Source <?> "local file or url?"
    , topn :: w ::: Maybe Int <?> "Top n word counts"
    , destinations :: w ::: Destinations <?> "output locations and format"
    }
    deriving (Generic)

instance Default Source where
    def = FileIn "other/files/f1.txt"

defUrl :: Source
defUrl = UrlIn "http://www.gutenberg.org/files/4300/4300-0.txt"

instance Default Destinations where
    def = Destinations [(FileOut "other/table.md", TableHtml), (ToStdout, Plain)]

instance Default (Opts w) where
    def = Opts (Just def) 10 (Just True) def

instance ParseField Source
instance ParseRecord Source
instance ParseField Destination
instance ParseRecord Destination
instance ParseField Destinations
instance ParseRecord Destinations
instance ParseRecord (Opts Wrapped)

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

wordStream :: Monad m => Int -> C.ByteString m r -> S.Stream (S.Of Text) m ()
wordStream n s =
    s &
    C.words &
    C.denull &
    S.take n &
    S.mapped C.toStrict &
    S.map ( decodeUtf8 >>>
            toLower >>>
            Text.split (not . (`Protolude.elem` ['a'..'z']))) &
    S.concat &
    S.filter (/="")

fwords :: L.Fold Text (Map Text Int)
fwords = L.Fold step Map.empty identity
  where
    step x a = Map.insertWith (+) a 1 x

faves :: Int -> Map Text Int -> [(Text,Int)]
faves n =
    Protolude.take n .
    sortBy (\(_,x) (_,y) -> compare y x) .
    Map.toList

fromUrl :: String -> IO (Map Text Int)
fromUrl f = do
    req <- BS.parseRequest f
    man <- BS.newManager BS.tlsManagerSettings
    BS.withHTTP req man $ \resp ->
        L.purely S.fold_ fwords (void $ wordStream 1000 (BS.responseBody resp))

mkTable :: [(Text,Int)] -> Text
mkTable ws = h <> sep <> b <> sep <> t
  where
    sep = "\n"
    h = "\n<table>"
    t = "</table>"
    b = Text.concat
        ((\x -> "<tr>\n" <> x <> "\n</tr>\n") .
          (\(w,n) -> "<th>" <> w <> "</th>\n" <> "<th>\n" <>
            show n <> "\n</th>\n") <$> ws)

runFaves :: Int -> C.ByteString (BS.ResourceT IO) () -> IO [(Text, Int)]
runFaves n s =
    fmap (faves n) $
    L.purely S.fold_ fwords (wordStream 10000 s) & BS.runResourceT

main :: IO ()
main = do
    o :: Opts Unwrapped <- unwrapRecord "counting words, haskell style"
    let n = fromMaybe 10 (number o)
    let input = fromMaybe def (source o)
    let output = fromMaybe def (destinations o)
    Protolude.putStrLn ("Top " <> show n <> " word counts ..." :: Text)
    ws <- runFaves n (C.readFile tFile)
    fmap doOutput output ws
      where
        doOutput (ToStdout, Plain) = Protolude.putStrLn . show
        doOutput (ToStdout, TableHtml) = Protolude.putStrLn . mkTable
        doOutput (FileOut f, Plain) = Protolude.writeFile f . show
        doOutput (FileOut f, TableHtml) = Protolude.writeFile f . mkTable

\end{code}

output
---

```include
other/answer.md
```

tests
---

- [doctest](https://www.stackage.org/package/doctest)

\begin{code}
-- | doctests
-- >>> let tFile = "other/files/f1.txt"
-- >>> ws <- runFaves 10 (C.readFile tFile)
-- [("et",182),("in",113),("est",70),("se",65),("ad",64),("ut",57),("numquam",50),("ne",45),("quod",44),("non",39)]
\end{code}

***

<div class="footer">
Powered by [haskell](https://haskell-lang.org/), [stack](https://docs.haskellstack.org/en/stable/README/) and [pandoc](http://pandoc.org/).
</div>
