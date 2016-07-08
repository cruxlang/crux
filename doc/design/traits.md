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

# Technical Notes

## Code Generation

* Terminology note: here, an "impl" is a pair of a trait and TypeVars.  It is also a runtime data structure consisting of a JS map of symbol name onto value.

* We have a global map of all trait impls
* In each scope, we must know the set of impls that are in scope.
* These pairs need to have a total ordering.  This allows us to desugar function calls and function definitions in any order.
* Only need to walk the function signature to come up with the parameter list.
* Tack this onto the crux->core step?  Gen.hs

* Collect the set of all trait impls and compile them somewhere
* We'll try putting them in the module where they are defined
    * If we do this, we need to know the module of each concrete trait impl for desugaring

* When compiling a function definition,
    * We must walk the type signature to know the set of typeclass-dictionaries we need.
    * We must sort this canonically, and pepend it onto the front of the argument list.
* When compiling a function call,
    * Walk its definition's type signature to know the set of typeclass dicts required
    * Walk the callsite's type signature to know how the type variable got filled in
    * For each type variable in the trait, we either
        * Grab the exact dictionary we need from the global set of trait impls, or
        * Grab the correct dictionary from the environment, in the case that we do not know the exact type in this scope
* When compiling a function reference (taking the value of a function)
    * If the type of the reference is less polymorphic than the function, we need to bind the relevant dictionaries here
    * If not, we don't need to do anything