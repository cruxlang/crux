# Angle Brackets for Type Parameters

## Philosophical or Stylistic Arguments

Rust, Swift, C#, and TypeScript all use angle brackets for generics, so they're quite familiar to many people.  For this reason, Chad has traditionally been in favor of angle brackets.

Andy, mdl, and imran have raised the objection that they're visually noisy and intimidating, especially compared to the generics of Haskell and ML which feel a lot lighter and more approachable.  I'm convinced a great deal of the fear of generics in the industry comes from peoples' past C++ experiences, and we all hope that generics in Crux are as pleasant as they are in Haskell.

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

Angle brackets have the nice property of being a familiar mechanism for introducing type variables into a context.  Related: explicit introduction of type variables, rather than implicit via case (Haskell) or via apostrophes (OCaml).  Explicit introduction has the nice property that machinery like ScopedTypeVariables is not necessary.

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

### Type-Level Comparison

Another downside of using angle brackets is that it makes the possible future feature of type-level comparison syntactically tricky.  `Foo<x < y>`.  We'd probably want to add a new token for that as a workaround.  I'm not aware of any prior art here besides C++, which has insane lexing and parsing rules.

## How important is this syntax?

I suspect that, in practice, this syntax is not that important.  Crux infers the most general type, and placeholder type variables avoid the need to spell out types all the time.

In addition, sugar for arrays, mutable arrays, and optionals means the angle brackets shouldn't show up too much in typical code.

# Your thoughts?

If you have any thoughts or feedback or ideas, please feel free to comment.
