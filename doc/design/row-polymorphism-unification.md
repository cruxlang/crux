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

1. Unify fields whose names coincide.
2. If the right-side record is free
    * Copy fields from the left side to the right side.
    * Rewrite the left-side type to be a `TBound` to the right-side type
    * Important question: Is it necessary for free row variables to support `TBound`?
3. If the left-side record is free
    * As in step 2, with the types reversed
4. If the left or right-side records are quantified
    * Fail unification unless both records' row variables are quantified with the same `QVar`
