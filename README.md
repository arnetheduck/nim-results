# Introduction

Result type that can hold either a value or an error, but not both

## Usage

Add the following to your `.nimble` file:

```
requires "result"
```

or just drop the file in your project!

## Example

```nim
import result

# It's convenient to create an alias - most likely, you'll do just fine
# with strings as error!

type R = Result[int, string]

# Once you have a type, use `ok` and `err`:

proc works(): R =
  # ok says it went... ok!
  R.ok 42
proc fails(): R =
  # or type it like this, to not repeat the type!
  result.err "bad luck"

if (let w = works(); w.ok):
  echo w[], " or use value: ", w.value

# In case you think your callers want to differentiate between errors:
type
  Error = enum
    a, b, c
  type RE[T] = Result[T, Error]

# In the expriments corner, you'll find the following syntax for passing
# errors up the stack:
proc f(): R =
  let x = ?works() - ?fails()
  assert false, "will never reach"
```

See result.nim for more in-depth documentation - specially towards the end where
there are plenty of examples!

## License

MIT license, just like Nim
