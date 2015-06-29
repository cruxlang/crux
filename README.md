
Crux is a principled, practical AltJS language.  It aims to provide a practical programming environment that doesn't compromise on strong fundamentals.

# Core Principles

* Hindley-Milner type inference
* Accessible to humans
* Small, fast code
* Clean mapping to JS
* Solid asynchronous programming

# Feature Wishlist

* Typeclasses
* The compiler should be fast

# What does it look like?

Current thinking here: [link](https://github.com/andyfriesen/Crux/wiki/Syntax-Strawman)

Summary:

```ocaml
data IntList {
    Cons Number IntList;
    Nil;
};

let rec len = fun l {
    match l {
        Nil => 0;
        Cons num tail => 1 + (len tail);
    };
};

let _ = print (len (Cons 5 Nil));
```

# Status

I give this project a 1% shot at eventually becoming useful enough to compile itself.  The whole effort is just little old me in my spare time.
