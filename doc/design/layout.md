# Rules

Whitespace-only lines are ignored, no matter how much whitespace.

LF and CRLF are both legal line endings.

Tabs are illegal in source code.

All top-level decls must be at indentation level 0.

* import
* export
* data
* let
* fun

The next line at indentation level 0 terminates the decl.

The bodies of import, export, data, let, fun blocks must be indented past their starting level, and all sublines must be at the same level.

Legal:

```
fun x() {
    y
}
```

```
let x = y
  + 2
```

```
export {
    foo,
    bar,
}
```

Illegal:

```
fun x() {
y
}
```

```
export {
foo,
bar,
}
```

```
let x = y +
2
```

```
fun
  x
  () {
}
```

Doesn't matter what line `{` starts on, but `}` must be at same indentation as the opening of the block.

# Algorithm

until eof:
    read decl
    assert indentation level of decl is 0

read decl:
    parse opening keyword from line
    parse identifier
