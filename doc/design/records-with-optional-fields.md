```
fun foo(bar, x): () {
    bar(x.optionalField)
}
```

We don't know what type `optionalField` has here.  It's fully unconstrained, except that `bar` takes it.

`foo`'s type is inferred to be `(T => (), R) => () where <T, R: {optionalField: T}>`

So should this be legal?

```
fun mybar(v) {
    print(v ? "default string")
}

foo(mybar, {})
foo(mybar, {optionalField: "given string"})
```

mybar has type `T => () where T: Optional<String>`

And when unifying the record type variable with the empty record, the type of the field is compatible (as long as it's a JSOption) and its value is None.

When unifying with the other field, it's a String.
