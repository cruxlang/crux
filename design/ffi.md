One key component of the FFI is specifying precisely how data structures are represented in JavaScript.  For example:

```
data Bool { False, True }
```

should be represented in JS with `false` and `true`.

Consider this example:

```
data ReadyState {
    UNSENT,
    OPENED,
    HEADERS_RECEIVED,
    LOADING,
    DONE,
}
```

The enum cases should be represented in JS as 0, 1, 2, 3, and 4 to match the XHR spec.

There are also enums that should be represented in JS as string constants.

Thus, we will need a mechanism for specifying the JavaScript representation of cases in sum types.

```
data jsffi Bool {
    True = true,
    False = false,
}

data jsffi ReadyState {
    UNSENT = 0,
    OPENED = 1,
    HEADERS_RECEIVED = 2,
    LOADING = 3,
    DONE = 4,
}
```

If jsffi is unspecified, simple enumerations compile into sequential integers (i.e. 0, 1, 2...)

Enumerations with associated data, as in the following example...

```
data Option a {
    None,
    Some(a),
}
```

... will compile into JavaScript something like this:

```
function Option() {}
function None() {}
None.prototype = Object.create(Option.prototype);
None.prototype.tag = 1;
function Some(a) {this.a = a;}
Some.prototype = Object.create(Option.prototype);
Some.prototype.tag = 2;
```

This allows for simple, efficient pattern matching by switching on the .tag property.  It also allows data to look good in the debugger without significant runtime overhead.

Note: to be even more efficient, we might consider representing values with a single no-data case as 'null'.  However, this is not very useful for the FFI: see the option type discussion below.

Many types will have direct, obvious representations in JavaScript.

Type | JS Type
-- | --
Int | Number truncated with &#124;0
Int | Number truncated with >>>0
Double | Number
String | String
functions | Function
() | undefined
records | Object
tuples | Array

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
