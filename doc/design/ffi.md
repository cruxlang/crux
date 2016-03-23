Crux is intended to be a practical, productive browser language.  As such, a clean FFI is an important
feature of its design.

## Primitives

Crux primitive data maps to JS data types relatively directly.

The Crux data types `Number`, `Boolean`, and `String` all directly map to the equivalent JS data types.

Crux's `Unit` type is always represented at runtime with `undefined` (where the generated code produces any
value at all)

Crux functions also map directly to JS functions at the moment. (this may conceivably change when certain language
features are introduced)

Crux records map directly to JS objects.  Property names are carried through as-written by the programmer.

## Enumerations

The `jsffi` keyword can be used to create enumeration data types that ascribe an exact JS representation
to each variant.  Because of this, `jsffi` enums obviously cannot have associated data.

```
data jsffi Bool {
    True = true,
    False = false,
};

data jsffi ReadyState {
    Unsent = 0,
    Opened = 1,
    HeadersReceived = 2,
    Loading = 3,
    Done = 4,
};
```

## Objects

Crux is not particularly object-oriented, but the browser API and popular JS frameworks are.  Crux affords a simple way
to use these APIs.

Crux records map exactly to JS objects.  Further, if an attribute of a record is a function, applying that function
is guaranteed to generate valid JS for a method call. (ie `this` will be properly set to a reference to the record)

There is a small sleight of hand going on here as we promise to the compiler that a value has properties which are
actually properties of the object's prototype.  Crux itself has no particular awareness of JS prototypes.

For instance, the following code can be used to fabricate an `XMLHttpRequest` object in Crux.

```js
data jsffi ReadyState {
    Unsent=0,
    Opened=1,
    HeadersReceived=2,
    Loading=3,
    Done=4,
};

data jsffi Method {
    Get="GET",
    Post="POST",
    Put="PUT",
    Delete="DELETE",
};

type Url = String;
type EventName = String;
type EventHandler = () -> Unit;

type XMLHttpRequest = {
    send : () -> Unit,
    open : (Method, Url) -> Unit,
    addEventListener : (EventName, EventHandler) -> Unit,
    setRequestHeader : (String, String) -> Unit,
    responseText : String,
};

fun newXhr() {
    let result : XMLHttpRequest = _unsafe_js("new XMLHttpRequest");
    result;
};

fun http_get(url, onLoaded) {
    let xhr = newXhr();

    let loadProc = fun() {
        onLoaded(xhr);
    };

    xhr.addEventListener("load", loadProc);
    xhr.open(Get, url);
    xhr.send();
    xhr;
};
```

## Future work

If jsffi is unspecified, we should represent simple enumerations as sequential integers (i.e. 0, 1, 2...)

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

Pros: It could concievably work.
Cons: Multi-param type classes are very heavyweight type system features.

# Symbol Naming

Crux values are alphanumeric and can never start with an underscore.  Importantly, Crux values cannot use the character `$` but JavaScript names can.

For sane debugging, values are mapped directly to the corresponding JS names as necessary.

## Special Cases

### Runtime Functions

Runtime functions can start with an underscore.  `_rts_set_current_exception`

### Module-Qualified References

Module-qualified references can be `$module$name_valueName`.

### Internally-Generated Names

Start with `$_`.  Temporaries are like `$0` or `$_0`.

### Shadowed Names When Targeting ES5 or Earlier

For shadowed names, we can either generate ES6 `let` constructs or do some light mangling like `_foo` or `foo2`.

### Names That Line Up With JS Keywords

Internally-generated names can start with `$_`.

### Exceptions

Exception constructors end with `$$`.  Exception FooError would be a `FooError$$` value in the generated JS.
