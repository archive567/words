stream-words
===

[![Build Status](https://travis-ci.org/tonyday567/stream-words.png)](https://travis-ci.org/tonyday567/stream-words)

See https://tonyday567.github.io/stream-words/index.html for project description.

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/stream-words-example" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i app/example.lhs -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~
