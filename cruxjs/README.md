# Crux.js

Crux.js is the Rust Crux compiler packaged as WebAssembly. It embeds the Crux
standard library and runtime, so compilation is synchronous and performs no
filesystem or network access.

The generated package exports two APIs:

- `compile(source)` returns generated JavaScript or throws a compiler error.
- `compileCrux(source)` preserves the historical API and returns an object
  containing either a `result` or `error` string.

## Build for browsers

Install the Wasm target and the binding generator version selected by
`Cargo.lock`, then run the build script from the repository root:

```sh
rustup target add wasm32-unknown-unknown
cargo install wasm-bindgen-cli --version 0.2.129 --locked
cruxjs/s/build
```

Browser artifacts are written to `cruxjs/dist`; CommonJS/npm artifacts are
written to `cruxjs/npm/cruxlang/src`.

Load and initialize the generated ES module before compiling:

```js
import init, { compile } from "./dist/cruxjs.js";

await init();
const javascript = compile(source);
```

## Build the npm CLI

```sh
cruxjs/s/build
node cruxjs/npm/cruxlang/cli.js program.cx
```

## Test

Native boundary and embedded-library tests:

```sh
cargo test --manifest-path cruxjs/Cargo.toml
```

For the complete native and Wasm/Node smoke test, install the build
prerequisites above and run:

```sh
cruxjs/s/test
```
