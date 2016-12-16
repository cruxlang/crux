# How to write a new npm module

```
mkdir npmtest
crux init
crux build
```

## Open Questions

* should crux distinguish between programs (automatically call main()) and libraries (just exports)?

## Obvious Improvements

* `crux init` should automatically create a skeleton project
* `crux build` should produce the 'build' directory if it doesn't exist
* add support for `export jsffi` to avoid accidentally exporting things that depend on traits
