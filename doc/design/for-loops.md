For loop syntax:

```
for <identifier> in <array-expr> {
    // <identifier> is available in here
}
```

This could easily be generalized in the future to:

```
for <pattern> in <value-of-type-with-iterable-trait> {
}
```

Where iterable returns an Option.

With inlining, it would be pretty straightforward to make something like:

```
for x in range(10) {
}
```

compile into:

```
for (let x = 0; x < 10; ++x) {
}
```

Until then, range() can return an array.
