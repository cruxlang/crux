
Crux is a principled systems language.  It is intended to be a superior solution
for applications that require high performance and reliability.  eg games, audio processing,
drivers.

Think "C with HM."

# High level

* Hindley-Milner type inference
* Sum types
* Precise control over memory
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
* Typeclasses can also be mechanically rewritten to use existentials to do runtime-polymorphic dispatch.
* Existentials for runtime polymorphism.