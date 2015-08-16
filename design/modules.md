# Circular Imports

Strawman: no circular imports allowed.

Dynamic languages (python, node, es6) get away with it by returning a partially-evaluated module.  C++ headers get away with support circular imports by "returning" a "partially-substituted" header.  Java gets away with it because there's no such thing as a "module" in Java.  Only classes and packages.

Ada, Go, Haskell, and OCaml don't support circular imports.  Crux is closer to these three in the programming language design space, so it makes sense it have these same restrictions.

A risk of not support circular imports: "Uh-Oh.  The recursive types across package boundaries issue again!  Run for it!!  :-)"
http://computer-programming-forum.com/44-ada/ce9b904c11b18fd6.htm

A common workaround that may not be so bad: http://stackoverflow.com/questions/36260/dealing-with-circular-dependencies-in-ocaml

Also google "untying the recursive knot" for many more discussions on this.


# ES6 syntax

```
export function square(x) { return x * x; }
export const sqrt = Math.sqrt;
export default <expression>;
```

Can have both default and non-default exports in a single file!  Non-default
exports become properties of the default export.

The default export is a special export simply named `default`.

Calls through ES6 module objects are statically resolved.

```
// Default exports and named exports
import theDefault, { named1, named2 } from 'src/mylib';
import theDefault from 'src/mylib';
import { named1, named2 } from 'src/mylib';

// Renaming: import named1 as myNamed1
import { named1 as myNamed1, named2 } from 'src/mylib';

// Importing the module as an object
// (with one property per named export)
import * as mylib from 'src/mylib';

// Only load the module, don’t import anything
import 'src/mylib';
```

```
export var myVar1 = ...;
export let myVar2 = ...;
export const MY_CONST = ...;

export function myFunc() {
    ...
}
export function* myGeneratorFunc() {
    ...
}
export class MyClass {
    ...
}
```

```
export * from 'src/other_module';
export { foo, bar } from 'src/other_module';

// Export other_module’s foo as myFoo
export { foo as myFoo, bar } from 'src/other_module';
```
