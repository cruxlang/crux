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
    render({props: ReactState<self>}): ReactElement
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
    fn render(c) {
        // c.props for ReactComponent<T> has type ReactState<T>
        // In this case, that works out to MyState

        let prop1 = c.props.prop1
        let prop2 = c.props.prop2

        ...
    }
}
```
