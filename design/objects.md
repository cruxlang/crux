Calling Crux an "Object Oriented" programming language is going a bit too far.  It's more diplomatic to say it's an "Object Having" language.

Objects in Crux map to bare JS objects.

Object types are _row polymorphic_, which means that the type is comprised of a set of name/type pairs that the object provides.

Object types are enclosed in curly braces.

```
type Point = {
    x : Number,
    y : Number,
};
```

(the final comma is optional)

The `.` operator is used to access a property of an object.

```
fun length(point) {
    sqrt(point.x * point.x + point.y + point.y);
};
```

Objects can be created with the same syntax you see in JS:

```
let mypoint = {
    x : 5,
    y : 2,
};
```

Again, the final comma is optional.

Row-polymorphism says that a function is typed according to its fields and their types.  The following is thus valid:

```
let point_one = { x : 5, y : 2 };
let named_point_two = { name : "The best point", x : 9, y : 3 };
fun distance(p1, p2) {
    let dx = p1.x - p2.x;
    let dy = p1.y - p2.y;
    sqrt(dx * dx + dy * dy);
};
print(distance(point_one, named_point_two));
```

In this example, the `distance` function can accept either point, because both are compatible with the type `{x:Number, y:Number}`.
