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




### Chad does some thinking

hit a snag on record traits

[8:16]  
trying to see if i can explain it

[8:16]  
so my goal is to make it possible to pass a concrete record into a function that takes a value that implements some trait

[8:16]  
like this:

[8:17]  
json.encode({ foo: "hello", bar: 10 })

[8:17]  
but let's think about the other direction:

[8:18]  
let r: {foo: String, bar: Number} = json.decode(somestr)

[8:19]  
we need to pass the appropriate instance dict into json.decode, which means we need to generate an instance dict

[8:19]  
which we can here: we know the concrete type

[8:20]  
the problem is, the impl syntax i've designed is one-directional

[8:20]  
basically it's sugar for transforming every property of an input record into a homogenous quantified record

[8:23]  
so the question is: where in the typechecker do we actually transform the input records

[8:24]  
we could do it at constraint validation time

[8:24]  
but that code only knows the _type_ of the record it's talking about, not the value

[8:24]  
so that seems wrong

[8:28]  
so what if i did it in the generated instance

[8:29]  
the instance by definition knows the closed set of properties and their types

[8:29]  
there is no polymorphism at that point

[8:29]  
actually that might not be true

[8:30]  
consider: `fun(x) { json.encode({x: x}) }`

[8:30]  
the instance there needs a parameter, which is the ToJSON dict for x

[8:34]  
ok, so when we take a closed record, the parameters to the instance are the instance dicts for the type parameters of the fields

[8:34]  
which we learn by unifying the field transformer across the concrete fields

[8:35]  
so in the `json.encode({x: x})` case, we unify the field transformer against the value x, and learn it has a constraint, and thus needs a dict passed into the instance

[8:35]  
maybe the dict constructor should just always take dicts, one for each field, in a dictionary itself

[8:37]  
back to the `fromJSON` case

[8:37]  
oh wait, before i talk about that, i need to think through the typevar side of things

[8:40]  
so a `toJSON {...}` instance has two steps

[8:41]  
1) map every field into a common type

[8:42]  
2) every method then takes a {...: T} for whatever common T

[8:42]  
ahhh i think i found my problem

[8:43]  
If {x: T} unifies with {...: T}, then {...: T} unifies with {x: T}

[8:43]  
which means i could perhaps write something like:

[8:43]  
fun f(): {...: Number} { ... }

[8:43]  
f().x

[8:43]  
let me see


github BOT [8:44 AM]  
[crux:record-traits] 1 new commit by Chad Austin:
`05e5040`  another checkpoint - Chad Austin 

chad [8:48 AM]  
so i can't break the typechecker today, but i don't understand why

[8:51]  
OKAY

[8:51]  
so the instance typechecker creates a quantified record type with unified fields

[8:51]  
ToJSON {...} {

[8:52]  
for fieldName { transform(fieldName) }

[8:52]  
the `self` type becomes {...: T}

[8:53]  
{...: T} only works properly for arguments, not for return values

[8:53]  
this makes it some kind of covariant or something