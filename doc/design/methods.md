C++ and Go and Java use `.` for both property accesses and method calls.  They can get away with this because they don't have bidirectional type inference.

For example, what type does `x` have here?

```
fun f(x) {
    return x.length();
}
```

Languages with bidirectional type inference (Haskell, ML) rely on unambiguous name lookup to accurately infer types.

```
import Data.HashMap.Strict as HM

f x = HM.length x
```

The problem is when you have all kinds of different data types: hash maps, tree maps, lists, arrays, strings.  They all have lengths, and it gets annoying to import a new module's length function every time you need to calculate a length.  (It's especially bad in Haskell where there is no consistent name for the function that calculates the length or size of a data structure.)

In dynamic languages like Python or JavaScript, you can type "x.length" but the cost there is that length is a dynamic lookup.  That is, methods have runtime overhead.

Can we achieve all of our goals?

1. bidirectional type inference when possible
2. no runtime overhead for method lookup
3. minimal boilerplate in method specification

Let's consider the Data.HashMap.Strict case above.  By importing Data.HashMap.Strict and using HM.length, we are implicitly giving the compiler the knowledge that x is a HashMap.  Verbose imports are strictly worse, from a human factors perspective, than simply annotating x to be a HashMap and then using that knowledge to lookup the appropriate length function.  Thus, we propose the following system:

```
fun f(x: HashMap) {
    return x->length;
}
```

The `->` operator intuitively means "given `x` of a known concrete type, look up the `length` function in the method set associated with the concrete type, and call it, without needing an explicit import".

If `x` has no explicit type annotation or otherwise-inferred concrete type, then usage of `->` is a typecheck error.

(We cannot use the `.` operator for method lookup because it conflicts with row polymorphism.  We hope this is easy to explain: `.` means "stored in" and `->` means "looked up via type".)

Open question: Should `x->length` be shorthand for `x->length()` or should it be a way to grab a reference to the appropriate (partially-applied?) function without calling it?  The latter would be consistent with Python's semantically beautiful descriptor mechanism, allowing `let y = x->length; y()` to be identical to `x->length()`.

One nuance is that methods are looked up from the outermost type of a generic type.  That is, methods can be used on a type like `Array a`, even if `a` is unknown.

I add the following constraint:

4. It's possible to define methods (of the same names) on two different types from one module.

This means we need to decouple method sets from modules.  (An earlier proposal suggested literally desugaring x->length as `Call (ResolvedReference (ModuleOf (TypeOf x)), "length") x` but then how would you distinguish between length for arrays and length for strings, if they're both defined in the prelude?)

I propose a very lightweight, vaguely go-ish, syntax:

```
data Array a {} // jsffi
export method each(arr: Array a, fun(a) -> ()): () {
    // ...
}
export method length(arr: Array a): int {
    // ...
}
```

`export` and `method` are orthogonal: `export` exposes the function to importers of this module, whereas `method` adds the function to the type's method set.

(One option is to have `method` imply `export`.  Given the `->` syntax's primary intent is to reduce import noise and improve human factors, perhaps it's useful for exported functions anyway.)
