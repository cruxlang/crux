So far Crux largely copies Haskell.

Packages are Uppercase.
Types are Uppercase
Constructors are Uppercase.

Type position vs. value position is unambiguous.

values are lowercase.
type variables are lowercase.

This makes parsing a bit less ambiguous, and avoids some table lookups.  But does it hurt the delightfulness and usability of the language?  I mean, humans don't care.  Might be nice to avoid a bunch of shift presses and allow lowercase packages and constructors and type names.
