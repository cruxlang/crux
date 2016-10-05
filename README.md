# What is Crux.js?

This project is the tooling that compiles the Crux compiler (written in Haskell) into a JavaScript binary that
can run in web browsers or via npm.  The native Crux compiler is much, much faster, but sometimes it's useful to run the
compiler from JavaScript.

Crux.js is built with GHCJS.


## How to compile

1. Get a version of `stack` that's new enough to install GHCJS.  Version 1.0.0 is new enough.
2. Install [sass](http://sass-lang.com/install)
3. Run `s/build`.  stack should download and compile everything you need.
4. The resulting JS is copied into the `stage/` directory alongside some scaffolding HTML and CSS.

## Deploying to website

Check out https://github.com/cruxlang/cruxlang.github.io

After building, copy the contents of stage/ into crux-web/try/

Commit and push.
