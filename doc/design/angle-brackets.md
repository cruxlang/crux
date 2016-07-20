# Angle Brackets for Type Parameters

## Philosophical Arguments

Rust, Swift, C#, and TypeScript all use angle brackets for generics, so they're quite familiar to many people.  For this reason, Chad has traditionally been in favor of angle brackets.

Andy, mdl, and imran have raised the objection that they're visually noisy and intimidating, especially compared to the generics of Haskell and ML which feel a lot lighter and more approachable.

Whatever the ultimate syntax, it should be consistent across both definition and use, and all language features.  Predictability is key.

## Practical Considerations

Syntactically, using whitespace to delimit type parameters is problematic in a few cases.

```
fun f(): Option {a: Number} {
}
```

The parser must determine whether the opening curly brace after Option is the beginning of the function body or a type parameter.  (We use a few tokens of lookahead to resolve that today.)

The type parameter list to a shorthand data type looks a bit strange when whitespace-delimited.

```
data Foo a(a)
```

Compare to:

```
data Foo<a>(a)
```

Angle brackets have the nice property of being a familiar location for introducing type variables into a context.

```
fun length<a>(arr: [a]): Int {
  ...
}
```

However, note that we plan to support type identifier wildcards, making that particular use of a type variable unnecessary:

```
fun length(arr: [_]): Int {
  ...
}
```

### Constraints

Qualifying type variables with constraints is a bit less syntactically ambiguous with explicit delimiters:

```
fun f<a: Eq + Show, b: Ord>(arr: [a]): [b] {
}

implement Eq Option<a: Eq> {
}
```

With whitespace, we can't use parens to group the constraints, as it would be ambiguous with the argument list.  We could perhaps have a separate constraint list, a la Haskell.

### HKTs

One downside of angle brackets is that it masks the concept of partial application of type functions - important when using higher-kinded types.

Consider a possible future:

```
data Map<key, value>

implement Functor Map<key> {
}
```

