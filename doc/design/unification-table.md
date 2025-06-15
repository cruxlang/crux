Record Unification

| A                      | B                 | Output                 |
| ---------------------- | ----------------- | ---------------------- |
| `{x: T, ...}`          | `{y: T, ...}`     | `{x: T, y: T, ...}`    |
| `{x: mutable T}`       | `{x: mutable? T}` | `{x: mutable T}`       |
| `{x: T}`               | `{x: mutable? T}` | `{x: T}`               |
| `{x: mutable? T, ...}` | `{…: T}`          | `{x: T, ... : T}`      |
| `{x: T, ...: T}`       | `{y: T, ...: T}`  | `{x: T, y: T, ...: T}` |

