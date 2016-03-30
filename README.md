# Crux web playground

This is a tiny GHCJS application that embeds the compiler in a webpage.

## How to compile

1. Get a version of `stack` that's new enough to install GHCJS.  Version 1.0.0 is new enough.
2. Install [sass](http://sass-lang.com/install)
3. Run `s/build`.  stack should download and compile everything you need.
4. The resulting JS is copied into the `stage/` directory alongside some scaffolding HTML and CSS.

## Deploying to website

Check out https://github.com/cruxlang/cruxlang.github.io

After building, copy the contents of stage/ into crux-web/try/

Commit and push.
