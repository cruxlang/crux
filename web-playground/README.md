# Crux web playground

This is a tiny GHCJS application that embeds the compiler in a webpage.

## How to compile

1. Get a very recent version of `stack`.  I used git revision `29f082ad4f51dd87c254687345ff6c1e5ea82bcb`.
2. Run `s/build`.  stack should download and compile everything you need.
3. The resulting JS is copied into the `stage/` directory alongside some scaffolding HTML and CSS.
