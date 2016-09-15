import "hint" HLint.Builtin.All
import "hint" HLint.Default
import Data.Maybe

{-
error = fromJust x ==> Data.Maybe.fromMaybe (error "A more informative error message") x
error = read x ==> Safe.readNote (error "A more informative error message") x
error = Debug.Trace.trace
error = (!!) a b
-}

ignore "Use camelCase" -- this is currently tripping on a lot of things. Reenable this and fix those, maybe.
ignore "Reduce duplication"
ignore "Use fewer imports"

ignore "Redundant do"
ignore "Use unless"
ignore "Use null"
ignore "Redundant =="
ignore "Redundant bracket"
ignore "Redundant do"
ignore "Use unless"
ignore "Use infix"
ignore "Use liftM"
ignore "Use isNothing"
ignore "Use if"
ignore "Eta reduce"
ignore "Use zipWith"
ignore "Redundant $"
ignore "Use import/export shortcut"
ignore "Use fromMaybe"
ignore "Use mapMaybe"
ignore "Use maybe"
ignore "Use list literal"
ignore "Use list literal pattern"
ignore "Use section"
ignore "Use second"
ignore "Use ***"
ignore "Use <>"
ignore "Use <$>"
ignore "Use ++"
ignore "Move brackets to avoid $"
ignore "Use String"
ignore "Use record patterns"
ignore "Use putStr"
ignore "Use list comprehension"
ignore "Use const"
ignore "Use guards"
ignore "Use :"
ignore "Use exitSuccess"
ignore "Use void"
