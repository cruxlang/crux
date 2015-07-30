
Crux is a principled, practical AltJS language that aims to provide an easy, familiar programming environment that
does not compromise on rock-solid fundamentals.

# Core Principles

* Hindley-Milner type inference
* Accessible to humans
* Small, fast code
* Clean mapping to JS
* Solid asynchronous programming

# Feature Wishlist

* Typeclasses
* The compiler should be fast
* Debugging should be pleasant

# What does it look like?

Current thinking here: [link](https://github.com/andyfriesen/Crux/wiki/Syntax-Strawman)

Summary:

```ocaml
data List a {
    Cons a (List a);
    Nil;
};

let s = Cons(5, Cons(6, Cons(7, Nil)));

let rec len = fun (list) {
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
* Row-polymorphic records

Not done:

* Modules
* Conditionals
* `break`, `continue`, `return`
* Mutability
* Asynchrony
* JS FFI
* Type aliasing
* Class definitions
