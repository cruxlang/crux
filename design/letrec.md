In OCaml, "let rec" is only valid for functions.  To avoid confusion, we'll say that all function definitions are at least self-recursive.  This is a human factors decision.

Now let's talk about mutual recursion.  It's pretty straightforward at the top level to allow mutual recursion between all functions in a module.  However, there are some intricacies regarding top-level value initializations and mutual recursion.  Differing from C, it would be beneficial for the language to define a global stable initialization order (remember: global initializers can call functions and have arbitrary side effects).  Lexical order would be best.  But consider:

```
fun a() { b(); };
let c = a();
let d = 99;
fun b() { d; };
```

If all functions in the module can be mutually self-recursive, but initialization of top-level values is performed lexically, then what value does 'd' have when a() is first called?

This same problem crops up in local scopes.  In OCaml, mutually recursive function definitions are grouped explicitly with `let rec ... and`.  However, we think additional syntax for grouping mutual recursion is not discoverable and certainly not familiar to people coming from Python and JavaScript where mutual recursion is generally trivial.  (These issues are likely where JavaScript's function hoisting and var rules originate from.)

The current proposal is the following:

1. Let bindings are visible below their definition.
2. Consecutive function definitions form a mutual recursion group.
3. Values are initialized in lexical order.
4. Circular module imports are disallowed.

Thus:

```
let a = 10;
fun b() { c(); } // legal, b and c form a recursion group
fun c() { a; } // legal, but calling e would be illegal
let d = b();
fun e() { 99; }
```

There is some risk that programmers will be confused by the introduction of a let between functions breaking mutual recursion.  However, mutual recursion ought to be rare, and allowing general mutual recursion between all functions in a scope leads to hairy issues like uninitialized values.
