# Exceptions

## Goals

- FFI compatibility with JS
- hierarchies are convenient if possible
- easy construction of user-defined exception types

throw :: Typeable a => a -> b
throw(foo())

tryCatch :: Typeable a => (() => b, (a) => b) => b

try {
    throw(foo())
}
catch (e: T) {
}
catch (g: T) {
}
finally {
}

Indexing catch on specific type doesn't let us catch a range of things.

Should we catch _qualified type variables_?

try {
    throw(foo())
}
catch<T, JSError T> (e: T) {
}

## Exceptions and FFI

```
catch <Crux Exception> <binding> {}
catch _ {}
```

Someday I think we do want an `Exception` trait that looks roughly like:

```
trait Exception {
    testException: a => ?self
}
```

The built-in `throw()` function would then have type `Exception t => t -> ()`.

It is in general unsafe for authors of exceptions to implement this trait directly, marking a type as an exception would be provide as a compiler built-in.
