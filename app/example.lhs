```include
other/header.md
```

stream-words
===

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

\begin{code}
import Protolude
import GHC.Base (String)
import Options.Generic
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
data Opts w = Opts
    { number :: w ::: Maybe Integer <?> "The number you want to product to"
    }
    deriving (Generic)

instance ParseRecord (Opts Wrapped)

site :: GHC.Base.String
site = "http://www.gutenberg.org/files/4300/4300-0.txt"

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)



wordStream :: Monad m => Int -> C.ByteString m r -> S.Stream (S.Of Text) m ()
wordStream n s =
    s  &
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

httpWords :: String -> IO (Map Text Int)
httpWords f = do
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
main :: IO ()
main = do
    -- ws <- httpWords "http://www.gutenberg.org/files/4300/4300-0.txt"
    let tFile = "other/files/f1.txt"
    Protolude.putStrLn ("Top 100 words being calced ..." :: Text)
    ws <- fmap (faves 100) $
        L.purely S.fold_ fwords (wordStream 10000 (C.readFile tFile)) & BS.runResourceT
    -- print $ faves 100 ws
    Protolude.writeFile "other/table.md" $
      mkTable ws

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
-- >>> 1 == 1
-- True
\end{code}

***

<div class="footer">
Powered by [haskell](https://haskell-lang.org/), [stack](https://docs.haskellstack.org/en/stable/README/) and [pandoc](http://pandoc.org/).
</div>
