# Rules

Whitespace-only lines are ignored, no matter how much whitespace.

The contents of parenthesized lists must be indented deeper than the line that started the expression or declaration.

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

```
fun foo(
    x,
) {
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

Every AST token is annotated with its line, and every line is annotated with its indentation level.  Newlines become tokens.

until eof:
    read decl
    assert indentation level of decl is 0

read decl:
    parse opening keyword from line
    parse identifier
    parse open paren or brace
