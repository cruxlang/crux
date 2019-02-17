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

### Walk through typechecking `ReactComponent.render`

The trait `ReactComponent` already defines `render` as `({props: ReactState<self>}) => ReactElement`.  Argument 0 is `self`, but needs an extra `FamilyConstraint` that points to `ReactState<self>`.  The constraint initially contains the identity to `ReactState`, plus the `TypeVar` referred to in the record's `props`.

In our actual implementation of `render`, we (I think) instantiate `render`.  This is perhaps delicate because we need to turn the QVar pointed at by the family constraint into a free typevar and preserve the identity between the constraint and the record type.

Once this is done, we unify `self` with `MyComponent`.  `self` has a family constraint on it, so we chase that, evaluate the family, and resolve the type of `MyComponent<self>`.

### Cases to consider

Say we have a type family `Foo`, and two free types `a` and `b`.

#### `a` could be unified before `b`

