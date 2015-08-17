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

### Strawman 1

Suppose we provide multi-parameter traits, we can provide a trait that permits values of one type, plus values of a type `JsNull` (whose sole inhabitant is `JsNull`)

```
data JsVal; // Opaque to Crux

trait Nullable fromType a {
    toJsVal : (a) -> JsVal;
};

data JsNull { JsNull; };
instance Nullable JsNull a {
    toJsVal : fun(_) -> { _unsafe_js("null"); }
};

instance Nullable a a {
    toJsVal : fun(a) -> { _unsafe_coerce(a); }
}
```

We can then write functions that look a bit like this: (I am devolving into Haskell because I have no idea what the
Crux syntax should be)

```haskell
do_jQuery_thing :: Nullable (Number -> Number -> IO ()) a => a -> IO ()
```

In this way, the first parameter to the `Nullable` trait hammers down what type of value may be used if `null` is not
desired.
