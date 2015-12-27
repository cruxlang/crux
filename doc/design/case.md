foo.bar and Foo.bar always parse to (lookup (identifier "foo") "bar") or (lookup (identifier "Foo") "bar"), respectively.

"foo" cannot be used to lookup a value.  That is, "import x; let y = x" is illegal.

Shadowing an import with a variable name will produce a warning.

In Haskell, constructors are capitalized but variables are lowercase.  This allows the meaning of a pattern match to be unambiguous.  In Crux, how will we know?

Consider:

```
// import type, but only one of its constructors
import { mod(type, c1) }

// ...

match x {
    c1 => 1;
    c2 => 2;
}
```

It's not obvious that `c1` is a constructor but `c2` is a variable in this context.  That said, the match is exhaustive, and if c2 is ever imported and it is no longer an exhaustive match, then the exhaustiveness check will fail.

If we removed the c1 import, the second match will fail because it covers the same space.

Let's look at a more complicated match:

```
match x {
    (c1, c2) => 1
    (c3, c4) => 2
    _ => 3
```

If c1 changes from a constructor to a variable, then the semantics of the match will change without the exhaustiveness or overlap detector failing.

It could be nice to introduce an explicit form for introducing a new binding pattern, though I don't have good ideas for what that syntax might be.  Maybe colon:

```
match x {
    c1 => 1
    :v => 2
}
```

I'm not a big fan of that.  Maybe let:

```
match x {
    (c1, let y) => 1
    (let x, c2) => 2
    Some(let z) => 3
}
```

Unlike Haskell, exhaustiveness check failures are errors, not warnings.

(An aside, we should totally copy Rust's match guard syntax.)
