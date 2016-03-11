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
