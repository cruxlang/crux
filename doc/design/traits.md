TRAITS

# Syntax

```
trait ToString ty {
    toString: fun(ty) -> String
}

trait Bounded ty {
    minBound : ty
    maxBound : ty
}
```

# Instance Selection

**TODO**

# TypeVar changes

TQuant grows set of constraints.

TUnbound also grows set of constraints!  Has important meaning:

* Free but constrained, or
* Quantified but constrained

When unifying free constrained type variables, combine constraints.

Quantified type variables remain frozen and must match exactly.

When instantiating a qvar that has constraints, yield an fvar with the same constraints.
