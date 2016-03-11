## "Class Syntax"

Even though Crux's roots are in functional programming in the style of ML or Haskell, it's intended to be approachable by anyone with a mainstream background.

It's possible to achieve something quite similar to class syntax with a bit of syntax sugar.

We can already write:

```
data Counter {
  Counter {
    mutable count: number,
  }
}

export fun newCounter(initial) {
  Counter({count: initial})
}

export fun increment(Counter this) {
  this.count += 1
}

export fun decrement(Counter this) {
  this.count -= 1
}


export fun getValue(Counter this) {
  this.count
}
```

Usage would look something like this:

```
import {
  counter(newCounter)
}

fun main() {
  let c = newCounter()
  c->increment()
  c->decrement()
  print(c->getValue())
}
```

However, let's look at the definition of the Counter type.

```
data Counter {
  Counter {
    mutable count: number,
  }
}
```

I don't like the double indentation or the duplicated type name.  Some possible solutions:

```
class Counter {
  mutable count: number;
}

data Counter: {
  mutable count: number;
}

struct Counter {
  mutable count: number;
}
```
