# Mutability in Crux

Crux is at its heart a practical, functional language, and first-class mutability is part of that.

Crux offers mutable data via two mechanisms:  let bindings and record fields.

## Mutable `let` Bindings

A mutable binding can be introduced with the syntax `let mutable`.  Mutable values can be updated with the `=` operator.

```js
let mutable x = value;
x = x + 5;
```

## Mutable record fields

Crux record fields can be denoted as mutable.  This is part of the row-variable.

Individual record fields can be made mutable with the `mutable` keyword.

```js
type MutablePoint = {
    mutable x : Number,
    mutable y : Number
};
```

You can also specifically denote an immutable record field with the `const` keyword.

```js
type ImmutablePoint = {
    const x : Number,
    const y : Number
};
```

We'll talk about why you might want to do this in the next section.

## Mutable Records and Type Inference

Crux's type checker infers mutability of record fields from how the records themselves are used.

A record field can be mutable, immutable, or _quantified_, with the following rules:

Mutable and immutable rows are never type-compatible.

If a function parameter is a record with a quantified field, either an immutable or a mutable field will work.

If the type solver cannot infer either mutability or immutability of a top-level system, it is quantified.
If you let the type solver do most of the heavy lifting, this will actually be the most common case.

For instance, this function will accept either mutable or immutable record fields for its argument:

```js
fun manhattan(p) { p.x + p.y; };
```
This function, however, requires a mutable `x` field.  `y` can either be mutable or immutable.
```js
fun add_y_into_x(p) { p.x = p.x + p.y; };
```
This function requires that both fields be mutable:
```js
fun swap_x_and_y(p) {
    let t = p.x;
    p.x = p.y;
    p.y = t;
};
```

## Mutable record fields in literal values

TODO: What to do here?

Strawman 1:
```js
let a = { mutable x : 33, const y : 1 };
```

Strawman 2: (no direct support; require a type annotation)
```js
let a : { mutable x : Number, const y : Number } = { x : 33, y : 0 };
```

## Mutability and Reference Semantics

Crux records are passed _by reference_.  This means that multiple names can in fact refer to the same record.

This aliasing is visible in the face of mutability.
```js
let a = {x:33};
let mutable b = a;
a.x = 443;
// b.x is also 443

b = {x:a.x};
a.x = 555;
// b.x is still 443
```

Another consequence of this is that a function can mutate fields of a record passed as an argument.
```js
fun move_to_origin(p) {
    p.x = 0;
    p.y = 0;
};
```

## Mutable fields in immutable records

The rule here is very simple: Mutability is in no way transitive.

For instance, it is ok to have a mutable binding to a record whose fields are all immutable.  Updating
the `let` binding is ok, but mutating the fields of the record is forbidden:
```js
let mutable p : {const x : Number} = {x: 55};
p.x = 22; // not ok!  x is const
p = {x:99}; // ok.  p is mutable
```

It is also ok to have an immutable binding to a record whose fields are mutable.
```js
let p = {x : 22};
p.x = 999; // ok
p = {x : 111}; // forbidden: p is not mutable
```
