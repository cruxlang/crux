Facts we can know about records

- A record has a field named `x` of type `y`
- The record field `x` is immutable.
- The record field `x` is mutable.
- The record has fields `x`, `y`, and `z` and perhaps more.
- The record has fields `x`, `y`, and `z` and no more.

```
fun f(x) {
    print(x.prop)
}
// x: {prop: mutable? a}
```

```
fun f(x) {
    x.prop = 10
}
// x: {prop: mutable Number}
```

## Record Unification

| A  | B  | Output
|---|---|---|---|---|
| {x: T, ...} | {y: T, ...} | {x: T, y: T, ...}
|   | aoeub
|   |   |
