
Crux is a principled systems language.  It is intended to be a superior solution
for applications that require high performance and reliability.  eg games, audio processing,
drivers.

Think "C with Hindley-Milner."

# Status

I started this like a week ago.  It's not even an executable proof of concept yet.

# High level

* Hindley-Milner type inference
* Sum types
* Precise, explicit control over memory
* No GC
* Small runtime
* Lean C FFI
* Optional unsafety
* Copy/move/destruction a la C++.

Key non-features

* Free of legacy boondoggles.
* No implicit coersions.

# Details

* HM types good
* Typeclasses offer statically dispatched ad-hoc polymorphism.
* Existentials are great for runtime polymorphism.
* Typeclasses can also be mechanically rewritten to use existentials to do runtime-polymorphic dispatch.
