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

Partially done:
* JS FFI
* Modules
* Exceptions

Not done:
* Asynchrony
* Class definitions
* Native code generation
* Interpreter
* Type classes / traits

# Compiling

The Crux compiler is a Haskell program, but you don't need to be a Haskell programmer to build or use it.

If you're familiar with building Haskell software, we expect the compiler to build either with cabal or stack.

If not, read on:

1. Get stack. https://docs.haskellstack.org/en/stable/README/
2. `git clone https://github.com/cruxlang/crux`
3. `cd crux`
4. `stack install`

This will build crux and install it to `~/.local/bin/crux`.

# A Tour of the Code

1. Lex.hs converts bytes into Tokens (see Token.hs)
2. Parse.hs converts Tokens into the AST (see AST.hs)
3. Typecheck.hs type-checks the AST, producing a typed AST (see TypeVar.hs and Unify.hs)
4. Gen.hs converts the AST into an IR
5. JSBackend.hs converts the IR into JS
6. JSTree.hs converts the JS AST into output bytes

Main.hs is the general command-line interface.

Look at Module.hs and Project.hs for the code to load modules and projects.
