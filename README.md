Crux is a principled, practical language that aims to provide an easy, familiar programming environment that
does not compromise on rock-solid fundamentals.

# Big Ideas

Crux is all about small, well-understood ideas that fit together without clumsy seams or weird corner cases.

We won't try to guess what you're trying to say because we don't think your customers should be the
ones to tell you that we guessed wrong.

Our intent with Crux is to capture

* the joy and human factors of Python
* the ability to run the same code on the frontend and backend
* the straightforward performance and operational semantics of ML
* the safety under overloading of Haskell
* and the lightweight M:N concurrency of Go, Haskell or Python

# What does it look like?

```ocaml
fun getFolderContents(folder) {
    let rv = []

    let contents: Array String = fs.readdirSync(folder)
    for entry in contents {
        let absEntry = combine(folder, entry)
        let stat = fs.lstatSync(absEntry)
        if stat.isFile() && (entry->endsWith(".jpeg") || entry->endsWith(".jpg")) {
            rv->append(entry)
        }
    }

    return rv
}
```

# Status

Working:
* Type inference
* Sums
* Pattern matching
* [Row-polymorphic records](https://github.com/andyfriesen/Crux/blob/master/doc/design/objects.md)
* `if-then-else`
* "imperative" control flow: `return`, `break`, `continue`
* Loops
* Type aliasing
* [Mutability](https://github.com/andyfriesen/Crux/blob/master/doc/design/mutability.md)
* "everything is an expression"
* Tail Calls
* Modules
* Exceptions
* Type classes / traits

Partially done:
* JS FFI

Not done:
* Asynchrony
* Class definitions
* Native code generation
* Interpreter

# Compiling

The compiler is implemented in Rust as the `crux` library crate. It includes
the lexer, source AST, bounded Chumsky parser, type checker, module loader, and
JavaScript backend:

```sh
cargo test
```

To run only the fixture-driven suite under `tests/integration`:

```sh
cargo test --test integration -- --nocapture
```

Fixtures run in parallel using Rayon's CPU-sized worker pool. Set
`RAYON_NUM_THREADS` to override the concurrency, for example
`RAYON_NUM_THREADS=4 cargo test --test integration -- --nocapture`.

The existing integration corpus is compiled and executed by the Rust test
suite. The old Haskell implementation remains temporarily as a behavioral
reference and can be checked with `stack test`.

The browser and npm compiler is implemented as a Rust/WebAssembly package in
`cruxjs`; see `cruxjs/README.md` for its build and JavaScript APIs.

# A Tour of the Code

1. `rust/src/lexer.rs` converts source text into positioned tokens.
2. `rust/src/chumsky_parser.rs` converts tokens into the source AST in `rust/src/ast.rs`.
3. `rust/src/typecheck.rs` performs Hindley-Milner inference, row unification,
   and trait checking.
4. `rust/src/codegen.rs` emits JavaScript and runtime trait dispatch.
5. `rust/src/compiler.rs` resolves imports, rejects cycles, links modules, and
   produces executable JavaScript.
6. `cruxjs/src/lib.rs` exposes the compiler to JavaScript through Wasm and
   embeds every module in `lib` at build time.

The public entry points are `crux::parse(file_name, source)`,
`crux::compiler::compile(file_name, source)`, and
`crux::compiler::compile_path(path)`. Wasm hosts use the filesystem-free
`crux::compiler::compile_in_memory` entry point.

The Chumsky parser enforces explicit token and nesting limits before entering
recursive combinators. The former hand-written parser remains temporarily as
`crux::parse_compat` for AST migration comparisons.
