
Crux is a principled, practical AltJS language that aims to provide an easy, familiar programming environment that
does not compromise on rock-solid fundamentals.

# Core Principles

* Hindley-Milner type inference
* Accessible to humans
* Small, fast code
* Clean mapping to JS
* Solid asynchronous programming

# What does it look like?

Current thinking here: [link](https://github.com/andyfriesen/Crux/wiki/Syntax-Strawman)

Summary:

```ocaml
data List a {
    Cons a (List a),
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
* [Row-polymorphic records](https://github.com/andyfriesen/Crux/blob/master/design/objects.md)
* `if-then-else`
* `return`
* Type aliasing
* [Mutability](https://github.com/andyfriesen/Crux/blob/master/design/mutability.md)

Not done:

* Modules
* Loops
* `break`, `continue`
* Exceptions
* JS FFI
* Asynchrony
* Class definitions
