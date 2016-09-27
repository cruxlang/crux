# The Problem

Traits on records are non-obvious.  I'm not aware of any prior art.

That said, there are some valid use cases.  Consider the following:

## JSON Literals

When encoding JSON it's very common to want to write:

```
json.encode({
  foo: "foo",
  bar: 10,
  baz: ("tuples", "are", "great"),
})
```

The desired implementation here is that `json.toJSON` is invoked
on every property, in effect producing a `Dict<json.Value>`.  Finally,
that Dict is coerced by the trait impl into a `json.Value` via `_unsafe_coerce`.
(It would be correct to call `json.toJSON` on the resulting dict but would make an
unnecessary conversion pass through the dict.)

## Structured Clone / Transferable

The DOM allows copying values from one context to another
via postMessage.  Only some types can be copied: null, boolean,
strings, numbers, objects, arrays, and various DOM data objects.

Things that can't be copied: errors, functions, tagged variants.

It's very common to want to write:

```
postMessage({
    foo: "foo",
    bar: 10,
})
```

This is shorthand for creating a dictionary and serializing it to js.Value.

The decoding side will receive a `Dict<Value>` and have to carefully decode it.

The desired implementation here is to have a `js.Coercible` trait with an extrinsic (non-method) function of type `<A: js.Coercible>(a: A) => Value` whose implementation is `_unsafe_coerce`.

Records implement `js.Coercible` if all of their properties are also `js.Coercible`.

## Dictionary Literals

Constructing dicts, and in the future various types of key-value maps, is annoying.
We should support dict/map literals via records.  Something like:

```
let d = dict.from({
    foo: 10,
    bar: 20,
})
// d has type Dict<Number>
```

The interesting thing here is that all the property types must be unified with each
other.  `bar: "str"` would be a type error.

The implementation of dict.from could be spelled something like:

```
fun from<P>(r: {...q where q: P}): Dict<P> {
    return _unsafe_coerce(r)
}
``` 

# A Strawman

TODO
