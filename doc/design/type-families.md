# Type Families

Type Families are a way to associate one type with another in an ad-hoc fashion.  They are like a type-level analogue to traits.

## Declaring

```
typefamily MyFamily

typeimpl MyFamily<Number> = Number
typeimpl MyFamily<String> = Number
typeimpl myFamily<Boolean> = String
```

## Use

```
// First, suppose we wrote a little React wrapper in Crux:

data ReactElement

typefamily ReactState

trait ReactComponent {
    render(self, {props: ReactState<self>}): ReactElement
}

// Here's how we'd author a component using this library:

data MyComponent()

data MyState {
    MyState({
        prop1: Number,
        prop2: Boolean
    })
}

typeimpl ReactState MyComponent = MyState

impl ReactComponent MyComponent {
    fn render(self, c) {
        // c.props for ReactComponent<T> has type ReactState<T>
        // In this case, that works out to MyState

        let prop1 = c.props.prop1
        let prop2 = c.props.prop2

        ...
    }
}
```

## Implementation

We'll use Haskell notation to describe the relation "`b` is the type associated with `a` through family `Foo`".  We'll write this as `b ~ Foo<a>`.

We add another kind of constraint to free and quantified type vars.  It says that there exists some other type that is the result of applying some type family.

```haskell
data FamilyConstraint = FamilyConstraint
    { fcIdentity :: TypeFamilyIdentity
    , fcResultType :: TypeVar
    }
```

The data structure here is a little bit kooky and nonintuitive!  The constraint says "I am constrained by `Foo<a>` for some other TypeVar `a`," and not "I constrain some other TypeVar."

We need a new TypeVar variant to capture an unapplied type family.  For now we'll call it `TPartialTypeFun` in the spirit of `TTypeFun`

```patch
  data TypeVar
      = TypeVar (IORef TypeState)
      | TQuant TypeSource ConstraintSet TypeNumber
      | TFun [TypeVar] TypeVar
      | TDataType (TDataTypeDef TypeVar)
      | TRecord [RecordField TypeVar]
      | TTypeFun [TypeVar] TypeVar
+     | TPartialTypeFun TTypeFamilyDef [TypeVar]
      deriving (Eq)
```

### Walk through typechecking `ReactComponent.render`

The trait `ReactComponent` already defines `render` as `({props: ReactState<self>}) => ReactElement`.  Argument 0 is `self`, but needs an extra `FamilyConstraint` that points to `ReactState<self>`.  The constraint initially contains the identity to `ReactState`, plus the `TypeVar` referred to in the record's `props`.

In our actual implementation of `render`, we (I think) instantiate `render`.  This is perhaps delicate because we need to turn the TQuant pointed at by the family constraint into a free typevar and preserve the identity between the constraint and the record type.

Once this is done, we unify `self` with `MyComponent`.  `self` has a family constraint on it, so we chase that, evaluate the family, and resolve the type of `MyComponent<self>`.

### Cases to consider

Say we have a type family `Foo`, and two free types `a` and `b`.

#### `a` could be unified before `b`

| a     | b     | a ∪ b
|------ | ----- | -----
| `free1` | `free2 ~ F2<b>` | easy
| Something concrete (like a function or a primitive) | `free2 ~ F2<b>` | Either the concrete type or a unification failure
| `free1 ~ F1<a>` | `free2 ~ F2<b>` | `free1 ~ F1<a> ~ F2<b>` (this typevar is constrained by two separate families)
| some table?   | `free2 ~ F2<b>` | ???

#### Now, what happens if we have a TypeVar `a ~ F<b>`, and then `b` becomes known?

This is the important part, and the part that requires that our data structure be "backwards".

When we say `a ~ F<b>`, we need to add a constraint no `b`, not `a`!!!

I think this means we need to link to the constraint from both ends. :|

## The constraint-argument needs to track the costrained free variable, and not the other way around.

```
typefamily Result

typeimpl Result<Number> = String

trait MyTrait {

    fn method(x: self) -> Result<Self>

}

let tr = method(some_value)
let some_value = 88
```

Before we check anything, the type environment is populated.
| | |
| --- | ---- |
| `method` | `<T>(self) -> Result<T>`
| `tr` | `f0`
| `some_value` | `f1`

Next, we start checking the `method` call.  We instantiate `method` (convert QVars to free vars)

`(f2,f3~Result<f2>) -> f3`  The constraint is attached to `f2`, the variable which is depended upon.

We unify this type with that of `some_value` and `tr` to arrive at

| | |
| --- | ---- |
| `method` | `<T>(self) -> Result<T>`
| `tr` | `f3`
| `some_value` | `f2,f3~Result<f2>`

Lastly, we unify the type of `some_value` (`f2`) with `Number`.  Since `f2` has a FamilyConstraint, we resolve `Result<Number>` to `String`, and unify.

| | |
| --- | ---- |
| `method` | `<T>(self) -> Result<T>`
| `tr` | `String`
| `some_value` | `Number`

Good!

# How de we store the type `(self) -> Result<Self>`?

```
TFun
    [TQuant {name="Self", constraints=[
        FamilyConstraint "Result" (TPartialTypeFun "Result" [TQuant "Self"])
    ]}]
    TTypeFun? [TQuant "Self"] (???? "Result")
```