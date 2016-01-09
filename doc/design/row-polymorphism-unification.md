# Row Polymorphism

Notation for this document

* `{a:A, b:B}` an exactly specified record.  All fields are specified.  No other fields exist.
* `{a:A, ...}` a *free* record.  Some fields are known, but others may yet be discovered as type inference progresses.
* `{a:A, ...q}` a *quantified* record.  Polymorphic: all possible records that include the fields given.

Helpful reminder: Unification is always reflexive and commutative

### Cases

> {...} with anything

Unify to anything.  Easy.

> {a:A,b:B} with {a:A, b:B}

No-op.  Easy.

> {a:A, ...} with {a:A, b:B}

Also easy.  lhs record becomes a TBound to the rhs.

Important: Also unify all the fields.

> {c:C, ...} with {a:A, b:B}

Unification error.

> {a:A, ...} with {b:B, ...}

Unify to `{a:A, b:B, ...}`

### Now with quantification:

> {a:A, b:B} with {a:A, ...q}

Unification failure.  Not all rhs records are the same shape as the lhs

> {a:A, ...} with {a:A, ...q}

Unifies to `{a:A, ...q}`

> {a:A, ...} with {a:A, b:B, ...q}

Unifies to `{a:A, b:B, ...q}`

> {a:A, ...q} with {a:A, ...p}

Unification failure.  There exists many pairs of row sets such that `...p /= ...q`

### Algorithm

First, unify intersecting fields, then:

C u C --> Fields must exactly coincide
C u F --> Fail if rhs has new properties.  rewrite rhs to be `TBound` to lhs
C u Q --> Fail.
F u F --> Copy lhs properties to rhs.  Rewrite lhs to be `TBound` to rhs
F u Q --> Fail if lhs has new properties.  Rewrite lhs to be `TBound` to rhs
Q u Q --> Fail unless `QVars` are equal
