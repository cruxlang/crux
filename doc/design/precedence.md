Sorted by descending precedence.

parens
call, method call, lookup    f(), f->b(), f.p
unary negation, not, complement    -x, !x, ~x
* /
+ -
-- TODO: make == and != lower precedence than <, <=, >, and >=
== != < > <= >=
-- TODO: bitwise
&& ||
assignment
if.then.else / match / while / for / return
semicolon ;
---

TODO: bitwise operations

if x then y else z + if a then b else c
if x then y else z + w
if if a + q then b else c then x else y
let x = 1 + let y = 2
