
Crux is a principled, practical AltJS language that aims to provide an easy, familiar programming environment that
does not compromise on rock-solid fundamentals.

# Big Ideas

Crux is all about small, clever ideas that fit together without clumsy seams or weird corner cases.  We won't try
to guess what you're trying to say because we don't think your customers should be the ones to tell you that we guessed
wrong.

* Easy, familiar syntax that doesn't hide surprises
* Hindley-Milner type inference
* Lean, obvious generated code
* Solid asynchronous programming

Our intent with Crux is to capture

* the joy and human factors of Python
* the ability to run the same code on the frontend and backend
* the straightforward performance and operational semantics of ML
* the safety under overloading of Haskell
* and the lightweight M:N concurrency of Go or Haskell or Python

# What does it look like?

Current thinking here: [link](https://github.com/andyfriesen/Crux/wiki/Syntax-Strawman)

Summary:

```ocaml
data List a {
    Cons(a, List a),
    Nil
};

let s = Cons(5, Cons(6, Cons(7, Nil)));

fun len(list) {
    match list {
        Nil => 0;
        Cons x tail => 1 + len(tail);
    };
};

let _ = print(len(s));
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

Not done:
* Exceptions
* Asynchrony
* Class definitions
* Native code generation
* Interpreter
* Type classes / traits
