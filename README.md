# Introduction

Result type that can hold either a value or an error, but not both

## Example

```nim
# It's convenient to create an alias - most likely, you'll do just fine
# with strings as error!
type R = Result[int, string]

# Once you have a type, use `ok` and `err`:
R.ok(42)
R.err("bad luck")

# In case you think your callers what to handle errors differently:
type
  Error = enum
    a, b, c
  type RE[T] = Result[T, Error]
```

See result.nim for more in-depth documentation!

## License

MIT license, just like Nim
