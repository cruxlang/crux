One key component of the FFI is specifying precisely how data structures are represented in JavaScript.  For example:

```
data Bool { False, True }
```

should be represented in JS with `false` and `true`.

Consider this example:

data ReadyState {
    UNSENT,
    OPENED,
    HEADERS_RECEIVED,
    LOADING,
    DONE,
}

The enum cases should be represented in JS as 0, 1, 2, 3, and 4 to match the XHR spec.

There are also enums that should be represented in JS as string constants.

Many types will have direct, obvious representations in JavaScript: Int, Double, functions.  Tuples can be represented as arrays.

## Option Types

Option types are tricky.

In simple cases, we could represent them as you'd expect in JavaScript.  For example, `Maybe Int` could be a number or `null`.  However, this strategy does not work in general.  `Maybe (Maybe Int)` has no obvious representation in JavaScript.  Does `null` mean `Nothing` or `Just Nothing`?  This is problematic in the case of generic code.  How should a `Maybe a` be represented?  Monomorphization of generic code would solve this problem but increase code size.

Andy's proposal is to implement a Nullable trait for types that can be converted to null.  [andy to flesh this out]
