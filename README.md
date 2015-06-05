
Crux is a principled, practical AltJS language.  It aims to sacrifice nothing.

# Core Principles

* Hindley-Milner static types all the way
* Accessible to humans
* Small code
* Fast code
* Clean mapping to raw JS
* Solid asynchronous programming

# Feature Wishlist

* Full static type inference
* Typeclasses
* The compiler should be fast

# What does it look like?

Current thinking here: [link](https://github.com/andyfriesen/Crux/wiki/Syntax-Strawman)

Summary:

```
let main = fun x {
    let spam = fun s {
        print s;
        print s;
        print s;
    };

    spam "spam";
    spam "spammity";
    spam "spam!!";
};

main 0;
```

# Status

I give this project a 1% shot at eventually becoming useful enough to compile itself.
