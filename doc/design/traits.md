TRAITS

# Syntax

```
trait ToString ty {
    fun toString(ty): String
}

trait Bounded ty {
    let minBound: ty
    let maxBound: ty
}
```

Note that the `fun` form does not name its parameters.  This is a one-off grammar production.

```
impl ToString Number {
    fun toString(n) { ... }
}
```

```
forall { t : ToString }
    fun join(delimiter : String, elements : Array t) { ... }
    
// or maybe

forall { ToString t }
    fun join(delimiter : String, elements : Array t) { ... }

```

# Associated Types

???

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
