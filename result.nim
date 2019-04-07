# Copyright (c) 2019 Jacek Sieka
# Licensed and distributed under either of
#   * MIT license (license terms in the root directory or at http://opensource.org/licenses/MIT).
#   * Apache v2 license (license terms in the root directory or at http://www.apache.org/licenses/LICENSE-2.0).
# at your option. This file may not be copied, modified, or distributed except according to those terms.

type
  Result*[T, E] = object
    ## Result type that can hold either a value or an error, but not both
    ##
    ## # Example
    ##
    ## ```
    ## # It's convenient to create an alias - most likely, you'll do just fine
    ## # with strings as error!
    ##
    ## type R = Result[int, string]
    ##
    ## # Once you have a type, use `ok` and `err`:
    ##
    ## proc works(): R =
    ##   # ok says it went... ok!
    ##   R.ok 42
    ## proc fails(): R =
    ##   # or type it like this, to not repeat the type!
    ##   result.err "bad luck"
    ##
    ## if (let w = works(); w.isOk):
    ##   echo w[], " or use value: ", w.value
    ##
    ## # In case you think your callers want to differentiate between errors:
    ## type
    ##   Error = enum
    ##     a, b, c
    ##   type RE[T] = Result[T, Error]
    ##
    ## # In the expriments corner, you'll find the following syntax for passing
    ## # errors up the stack:
    ## proc f(): R =
    ##   let x = ?works() - ?fails()
    ##   assert false, "will never reach"
    ##
    ## ```
    ##
    ## # Potential benefits:
    ##
    ## * Handling errors becomes explicit and mandatory - goodbye "out of sight,
    ##   out of mind"
    ## * Errors are a visible part of the API - when they change, so must the
    ##   calling code and compiler will point this out - nice!
    ## * Errors are a visible part of the API - your fellow programmer is
    ##   reminded that things actually can go wrong
    ## * Jives well with Nim `discard`
    ## * Jives well with the new Defect exception hierarchy, where defects
    ##   are raised for unrecoverable errors and the rest of the API uses
    ##   results
    ## * Error and value return have similar performance characteristics
    ## * Caller can choose to turn them into exceptions at low cost - flexible
    ##   for libraries!
    ## * Mostly relies on simple Nim features - though this library is no
    ##   exception in that compiler bugs were discovered writing it :)
    ##
    ## # Potential costs:
    ##
    ## * Handling errors becomes explicit and mandatory - if you'd rather ignore
    ##   them or just pass them to some catch-all, this is noise
    ## * When composing operations, value must be lifted before processing,
    ##   adding potential verbosity / noise (fancy macro, anyone?)
    ## * There's no call stack captured by default (see also `catch` and
    ##   `capture`)
    ## * The extra branching may be more expensive for the non-error path
    ##   (though this can be minimized with PGO)
    ##
    ## The API visibility issue of exceptions can also be solved with
    ## `{.raises.}` annotations - as of now, the compiler doesn't remind
    ## you to do so, even though it knows what the right annotation should be.
    ##
    ## Many system languages make a distinction between errors you want to
    ## handle and those that are simply bugs or unrealistic to deal with..
    ## handling the latter will often involve aborting or crashing the process -
    ## reliable systems like Erlang will try to relaunch it.
    ##
    ## On the flip side we have dynamic languages like python where there's
    ## nothing exceptional about exceptions (hello StopIterator). Python is
    ## rarely used to build reliable systems - its strengths lie elsewhere.
    ##
    ## # Other languages
    ##
    ## Result-style error handling seems pretty popular lately, specially with
    ## statically typed languages:
    ## Haskell: https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Either.html
    ## Rust: https://doc.rust-lang.org/std/result/enum.Result.html
    ## Modern C++: https://github.com/viboes/std-make/tree/master/doc/proposal/expected
    ## More C++: https://github.com/ned14/outcome
    ##
    ## Swift is interesting in that it uses a non-exception implementation but
    ## calls errors exceptions and has lots of syntactic sugar to make them feel
    ## that way by implicitly passing them up the call chain - with a mandatory
    ## annotation that function may throw:
    ## https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/ErrorHandling.html
    ##
    ## # Other implemenations in nim
    ##
    ## There are other implementations in nim that you might prefer:
    ## * Either from nimfp: https://github.com/vegansk/nimfp/blob/master/src/fp/either.nim
    ## * result_type: https://github.com/kapralos/result_type/
    ##
    ## # Implementation notes
    ##
    ## This implementation is mostly based on the one in rust. Compared to it,
    ## there are a few differences - if know of creative ways to improve things,
    ## I'm all ears.
    ##
    ## * Rust has the enum variants which lend themselves to nice construction
    ##   where the full Result type isn't needed: `Err("some error")` doesn't
    ##   need to know value type - maybe some creative converter or something
    ##   can deal with this?
    ## * Nim templates allow us to fail fast without extra effort, meaning the
    ##   other side of `and`/`or` isn't evaluated unless necessary - nice!
    ## * In Nim, we have exceptions - when using this library, we'll raise the
    ##   standard crop of Nim errors when trying to access the error of a value
    ##   and vice versa - this fits better with Nim but costs space and
    ##   performance - need to think about this - rust will simply panic, and
    ##   everyone seems more or less happy with that..
    ## * Rust uses From traits to deal with result translation as the result
    ##   travels up the call stack - needs more tinkering - some implicit
    ##   conversions would be nice here

    case isOk*: bool
    of false:
      error*: E
    of true:
      value*: T

  # TODO cannot raise generic error, so cannot include error value :(
  # https://github.com/nim-lang/Nim/issues/7845
  ResultError = object of Exception

proc ok*(r: typedesc[Result],  v: auto): r =
  ## Initialize a result with a success and value
  ## Example: `Result[int, string].ok(42)`
  r(isOk: true, value: v)

proc ok*(self: var Result, v: auto) =
  ## Set the result to success and update value
  ## Example: `result.ok(42)`
  self = Result.ok(v)

proc err*(r: typedesc[Result], e: auto): r =
  ## Initialize the result to an error
  ## Example: `Result[int, string].err("uh-oh")`
  r(isOk: false, error: e)

proc err*(self: var Result, v: auto) =
  ## Set the result as an error
  ## Example: `result.err("uh-oh")`
  self = Result.err(v)

template isErr*(self: Result): bool = not self.isOk

proc map*[T, E, A](self: Result[T, E], f: proc(x: T): A {.closure.}): Result[A, E] =
  ## Transform value using f, or return error
  if self.isOk: result.ok(f(self.value))
  else: result.err(self.error)

proc mapErr*[T, E, A](self: Result[T, E], f: proc(x: E): A {.closure.}): Result[T, A] =
  ## Transform error using f, or return value
  if self.isOk: result.ok(self.value)
  else: result.err(f(self.error))

proc mapConvert*[T0, E0](self: Result[T0, E0], T1: typedesc): Result[T1, E0] =
  ## Convert result value to A using an implicit conversion
  ## Would be nice if it was automatic...
  if self.isOk: result.ok(self.value)
  else: result.err(self.error)

proc mapCast*[T0, E0](self: Result[T0, E0], T1: typedesc): Result[T1, E0] =
  ## Convert result value to A using a cast
  ## Would be nice with nicer syntax...
  if self.isOk: result.ok(cast[T1](self.value))
  else: result.err(self.error)

template `and`*(self: Result, other: untyped): untyped =
  ## Evaluate `other` iff self.isOk, else return error
  ## fail-fast - will not evaluate other if a is an error
  if self.isOk:
    other
  else:
    type R = type(other)
    R.err(self.error)

template `or`*(self: Result, other: untyped): untyped =
  ## Evaluate `other` iff not self.isOk, else return self
  ## fail-fast - will not evaluate other if a is a value
  if self.isOk: self
  else: other

template catch*(body: typed): Result[type(body), ref Exception] =
  ## Convert a try expression into a Result
  type R = Result[type(body), ref Exception]

  try:
    R.ok(body)
  except:
    R.err(getCurrentException())

template capture*(a: typedesc, e: ref Exception): Result[a, ref Exception] =
  type R = Result[a, ref Exception]

  var ret: R
  try:
    # TODO is this needed? I think so, in order to grab a call stack, but
    #      haven't actually tested...
    if true:
      # I'm sure there's a nicer way - this just works :)
      raise e
  except:
    ret = R.err(getCurrentException())
  ret

proc `==`(lhs, rhs: Result): bool =
  if lhs.isOk != rhs.isOk:
    false
  elif lhs.isOk:
    lhs.value == rhs.value
  else:
    lhs.error == rhs.error

proc `[]`*(self: Result): auto =
  ## Fetch value of result if set, or raise error as an Exception
  if self.isErr:
    var e: ref ResultError
    when compiles($e.error):
      # Set exception message, iff we can convert error to a string
      new (e, $e.error)
    else:
      new (e)
    raise e
  self.value

proc `[]`*[T](self: Result[T, ref Exception]): T =
  ## Fetch value of result if set, or raise the contained exception
  if self.isErr:
    raise self.error
  self.value

proc `$`*(self: Result): string =
  ## Returns string representation of `self`
  if self.isOk: "Ok(" & $self.value & ")"
  else: "Err(" & $self.error & ")"

template valueOr*[T, E](self: Result[T, E], def: T): T =
  ## Fetch value of result if set, or supplied default
  ## default will not be evaluated iff value is set
  if self.isErr: def
  else: self.value

when isMainModule:
  type R = Result[int, string]

  # Basic usage, producer
  proc works(): R =
    R.ok(42)
  proc fails(): R =
    R.err("dummy")

  proc works2(): R =
    result.ok(42)
  proc fails2(): R =
    result.err("dummy")

  proc raises(): int =
    raise newException(Exception, "hello")

  # Basic usage, consumer
  let
    a = works()
    b = fails()

  doAssert a.isOk
  doAssert a.value == 42
  doAssert (not a.isErr)

  # Combine
  let xxx = a and b
  doAssert (not xxx.isOk)
  doAssert (a or b).isOk

  # Exception on access
  let va = try: discard a.error; false except: true
  doAssert va, "not an error, should raise"

  # Exception on access
  let vb = try: discard b.value; false except: true
  doAssert vb, "not an error, should raise"

  var x = a

  # Mutate
  x.err("failed now")

  doAssert x.isErr

  # Exceptions -> results
  let c = catch:
    raises()

  doAssert c.isErr

  # De-reference
  try:
    echo b[]
    doAssert false
  except:
    discard

  doAssert a.valueOr(50) == a.value
  doAssert b.valueOr(50) == 50

  # Comparisons
  doAssert (works() == works2())
  doAssert (fails() == fails2())
  doAssert (works() != fails())

  var counter = 0
  proc incCounter(): R =
    counter += 1
    R.ok(counter)

  doAssert (b and incCounter()).isErr, "b fails"
  doAssert counter == 0, "should fail fast on b"

  # Mapping
  doAssert (a.map(proc(x: int): string = $x)[] == $a.value)
  doAssert (b.mapErr(proc(x: string): string = x & "no!").error == (b.error & "no!"))

  # Exception interop
  let e = capture(int, newException(Exception, "test"))
  doAssert e.isErr
  try:
    discard e[]
    doAssert false, "should have raised"
  except:
    doAssert getCurrentException().msg == "test"

  # Nice way to checks
  if (let v = works(); v.isOk):
    doAssert v[] == v.value

  doAssert $a == "Ok(42)"

  doAssert a.mapConvert(int64)[] == int64(42)
  doAssert a.mapCast(int8)[] == int8(42)

  # TODO there's a bunch of operators that one could lift through magic - this
  #      is mainly an example
  template `+`*(self, other: Result): untyped =
    ## Perform `+` on the values of self and other, if both are ok
    type R = type(other)
    if self.isOk:
      if other.isOk:
        R.ok(self.value + other.value)
      else:
        R.err(other.error)
    else:
      R.err(self.error)

  # Simple lifting..
  doAssert (a + a)[] == a.value + a.value

  iterator items[T, E](self: Result[T, E]): T =
    ## Iterate over result as if it were a collection of either 0 or 1 items
    ## TODO should a Result[seq[X]] iterate over items in seq? there are
    ##      arguments for and against
    if self.isOk:
      yield self.value

  # Iteration
  var counter2 = 0
  for v in a:
    counter2 += 1

  doAssert counter2 == 1, "one-item collection when set"

  # Technically, it's possible to make a template that fetches type from
  # result - whether this is a good idea is up for discussion:
  template ok(v: untyped) {.dirty.} =
    result.ok(v)
    return

  proc testOk(): Result[int, string] =
    ok 42

  doAssert testOk()[] == 42

  # It's also possible to use the same trick for stack capture:
  template capture*(): untyped =
    type R = type(result)

    var ret: R
    try:
      # TODO is this needed? I think so, in order to grab a call stack, but
      #      haven't actually tested...
      if true:
        # I'm sure there's a nicer way - this just works :)
        raise newException(Exception, "")
    except:
      ret = R.err(getCurrentException())
    ret

  proc testCapture(): Result[int, ref Exception] =
    return capture()

  doAssert testCapture().isErr

  template `?`[T, E](self: Result[T, E]): T =
    ## Early return - if self is an error, we will return from the current
    ## function, else we'll move on..
    let v = self
    if v.isErr: return v

    v.value

  proc testQn(): Result[int, string] =
    let x = ?works() - ?works()
    result.ok(x)

  proc testQn2(): Result[int, string] =
    # looks like we can even use it creatively like this
    if ?fails() == 42: raise newException(Exception, "shouldn't happen")

  doAssert testQn()[] == 0
  doAssert testQn2().isErr
