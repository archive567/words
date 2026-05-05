# words

Streaming word counting and frequency analysis from files and URLs.

## Features

- **fromUrl** — fetch and analyze text from a URL (Project Gutenberg, etc.)
- **fromFile** — count words from a local file
- **fromUrlFreq** — stream a URL and return word frequency map
- **wordStream** — convert ByteString to lowercased, filtered word stream
- **wordCount** — fold to accumulate word frequencies as a Map

## Usage

Count word frequencies from Project Gutenberg (Alice in Wonderland):

```haskell
import Words

result <- fromUrlFreq "http://www.gutenberg.org/files/4300/4300-0.txt"
take 10 . sortBy (comparing (Down . snd)) . Map.toList $ result
```

Output:
```
[("the",551),("and",308),("a",255),("of",247),("his",191),("he",190),("to",180),("in",170),("said",166),("i",151)]
```

Or from a local file:

```haskell
frequencies <- fromFile "alice.txt"
```

See haddock documentation for full API.
