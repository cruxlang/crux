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

# Thoughts

```
let r = {x: 10}
let d = dict.from(r)
d.delete('x')
r.x
```

This is not sound, so dict.from must copy in general.
(We could have an unsafeFrom or special-case literals.)

The optimizer could elide the copy, so it's not worth messing
with language semantics there.

What are things we'd like to say about constrained records?  The
only possibility is "all field types have this constraint".

The two possible constraints are "all fields have this type" and
"all fields implement this trait".

There are three things we might want to do with such a record:

1. make a field-wise copy (if coercing into a mutable data structure)
2. unsafe coerce into something like a js.Value
3. make a copy, applying a function to each value

### Unified Properties

Whether copying or not, the first two can be solved with a special
type of record annotation:

```
let r = {x: 10, y: 20}
let p: {... =Number} = r
```

They unify, but `r` and `p` remain different types.  The only thing
that can be done with p is to coerce it (perhaps via something like
`dict.from`.)

### Constrained Properties

Now let's consider the case where we want a constraint on each property.

```
let r = {x: 10, y: "hello"}
let p: {... :ToString} = r
```

This is _not_ a coercion - code would need to be generated to wrap
each value in a ToString existential.  So this case is equivalent to
the map case.

### Mapping Each Property

Let's consider the case above and the JSON case: we want to write

```
json.encode({
  foo: 10,
  bar: "hello",
  baz: (True, js.Null)
})
```

Each property's TypeVar is independent, and in general can't
be coerced directly into a json.Value.  That is, each property's
value must be run through json.toJSON.  This is not a simple
unification - code must be generated.

Unification never produces code in general.  (Perhaps it's
possible but we don't yet have the algebra for that.)
How could this work then?

json.encode accepts a trait dict, and we could produce a trait
impl for this record:

```
impl json.ToJSON {...: json.ToJSON} {
  fieldTransform = json.toJSON
  toJSON(record: {... =json.Value}) = json.toJSON(dict.from(record))
  finalize({... :Value}) = json.toJSON(dict.from(x))
}
```

Generating an instance for a record would look up the fieldTransform,
_statically_ apply it to all the fields (instantiating each time),
unify all the result type vars, and then run finalize.

We'd generate a new ToJSON impl for each record, but that could be
inlined out.

### TODO: js.Coercible

### TODO: mutability

