# Spin: build, polish, lint, seal, and send.

default:
    @just --list

build:
    cabal build

polish:
    find src app -name '*.hs' -exec ormolu -i {} + 2>/dev/null || find src -name '*.hs' -exec ormolu -i {} +

lint:
    hlint src/ app/ 2>/dev/null || hlint src/

check: build polish lint

seal msg:
    git add -A
    git commit -m "{{msg}}"

spin msg: check
    git add -A
    git commit -m "{{msg}}"
    git push
