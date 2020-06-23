# Copyright (c) 2019 Jacek Sieka
# Licensed and distributed under either of
#   * MIT license (license terms in the root directory or at http://opensource.org/licenses/MIT).
#   * Apache v2 license (license terms in the root directory or at http://www.apache.org/licenses/LICENSE-2.0).
# at your option. This file may not be copied, modified, or distributed except according to those terms.

type
  ResultError*[E] = object of ValueError
    ## Error raised when using `tryGet` value of result when error is set
    ## See also Exception bridge mode
    error*: E

  ResultDefect* = object of Defect
    ## Defect raised when accessing value when error is set and vice versa
    ## See also Exception bridge mode

  Result*[T, E] = object
    ## Result type that can hold either a value or an error, but not both
    ##
    ## # Example
    ##
    ## ```
    ## # It's convenient to create an alias - most likely, you'll do just fine
    ## # with strings or cstrings as error
    ##
    ## type R = Result[int, string]
    ##
    ## # Once you have a type, use `ok` and `err`:
    ##
    ## func works(): R =
    ##   # ok says it went... ok!
    ##   R.ok 42
    ## func fails(): R =
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
    ## func f(): R =
    ##   let x = ?works() - ?fails()
    ##   assert false, "will never reach"
    ##
    ## # If you provide this exception converter, this exception will be raised
    ## # on dereference
    ## func toException(v: Error): ref CatchableError = (ref CatchableError)(msg: $v)
    ## try:
    ##   RE[int].err(a)[]
    ## except CatchableError:
    ##   echo "in here!"
    ##
    ## ```
    ##
    ## See the tests for more practical examples, specially when working with
    ## back and forth with the exception world!
    ##
    ## # Potential benefits:
    ##
    ## * Handling errors becomes explicit and mandatory at the call site -
    ##   goodbye "out of sight, out of mind"
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
    ## `{.raises.}` does not participate in generic typing, making it just as
    ## verbose but less flexible in some ways, if you want to type it out.
    ##
    ## Many system languages make a distinction between errors you want to
    ## handle and those that are simply bugs or unrealistic to deal with..
    ## handling the latter will often involve aborting or crashing the funcess -
    ## reliable systems like Erlang will try to relaunch it.
    ##
    ## On the flip side we have dynamic languages like python where there's
    ## nothing exceptional about exceptions (hello StopIterator). Python is
    ## rarely used to build reliable systems - its strengths lie elsewhere.
    ##
    ## # Exception bridge mode
    ##
    ## When the error of a `Result` is an `Exception`, or a `toException` helper
    ## is present for your error type, the "Exception bridge mode" is
    ## enabled and instead of raising `ResultError`, `tryGet` will raise the
    ## given `Exception` on access. `[]` and `get` will continue to raise a
    ## `Defect`.
    ##
    ## This is an experimental feature that may be removed.
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
    ## # Considerations for the error type
    ##
    ## * Use a `string` or a `cstring` if you want to provide a diagnostic for
    ##   the caller without an expectation that they will differentiate between
    ##   different errors. Callers should never parse the given string!
    ## * Use an `enum` to provide in-depth errors where the caller is expected
    ##   to have different logic for different errors
    ## * Use a complex type to include error-specific meta-data - or make the
    ##   meta-data collection a visible part of your API in another way - this
    ##   way it remains discoverable by the caller!
    ##
    ## A natural "error API" progression is starting with `Option[T]`, then
    ## `Result[T, cstring]`, `Result[T, enum]` and `Result[T, object]` in
    ## escalating order of complexity.
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
    ## * Rust uses From traits to deal with result translation as the result
    ##   travels up the call stack - needs more tinkering - some implicit
    ##   conversions would be nice here
    ## * Pattern matching in rust allows convenient extraction of value or error
    ##   in one go.
    ##
    ## # Performance considerations
    ##
    ## When returning a Result instead of a simple value, there are a few things
    ## to take into consideration - in general, we are returning more
    ## information directly to the caller which has an associated cost.
    ##
    ## Result is a value type, thus its performance characteristics
    ## generally follow the performance of copying the value or error that
    ## it stores. `Result` would benefit greatly from "move" support in the
    ## language.
    ##
    ## In many cases, these performance costs are negligeable, but nonetheless
    ## they are important to be aware of, to structure your code in an efficient
    ## manner:
    ##
    ## * Memory overhead
    ##   Result is stored in memory as a union with a `bool` discriminator -
    ##   alignment makes it somewhat tricky to give an exact size, but in
    ##   general, `Result[int, int]` will take up `2*sizeof(int)` bytes:
    ##   1 `int` for the discriminator and padding, 1 `int` for either the value
    ##   or the error. The additional size means that returning may take up more
    ##   registers or spill onto the stack.
    ## * Loss of RVO
    ##   Nim does return-value-optimization by rewriting `proc f(): X` into
    ##   `proc f(result: var X)` - in an expression like `let x = f()`, this
    ##   allows it to avoid a copy from the "temporary" return value to `x` -
    ##   when using Result, this copy currently happens always because you need
    ##   to fetch the value from the Result in a second step: `let x = f().value`
    ## * Extra copies
    ##   To avoid spurious evaluation of expressions in templates, we use a
    ##   temporary variable sometimes - this means an unnecessary copy for some
    ##   types.
    ## * Bad codegen
    ##   When doing RVO, Nim generates poor and slow code: it uses a construct
    ##   called `genericReset` that will zero-initialize a value using dynamic
    ##   RTTI - a process that the C compiler subsequently is unable to
    ##   optimize. This applies to all types, but is exacerbated with Result
    ##   because of its bigger footprint - this should be fixed in compiler.
    ## * Double zero-initialization bug
    ##   Nim has an initialization bug that causes additional poor performance:
    ##   `var x = f()` will be expanded into `var x; zeroInit(x); f(x)` where
    ##   `f(x)` will call the slow `genericReset` and zero-init `x` again,
    ##   unnecessarily.
    ##
    ## Comparing `Result` performance to exceptions in Nim is difficult - it
    ## will depend on the error type used, the frequency at which exceptions
    ## happen, the amount of error handling code in the application and the
    ## compiler and backend used.
    ##
    ## * the default C backend in nim uses `setjmp` for exception handling -
    ##   the relative performance of the happy path will depend on the structure
    ##   of the code: how many exception handlers there are, how much unwinding
    ##   happens. `setjmp` works by taking a snapshot of the full CPU state and
    ##   saving it to memory when enterting a try block (or an implict try
    ##   block, such as is introduced with `defer` and similar constructs).
    ## * an efficient exception handling mechanism (like the C++ backend or
    ##   `nlvm`) will usually have a lower cost on the happy path because the
    ##   value can be returned more efficiently. However, there is still a code
    ##   and data size increase depending on the specific situation, as well as
    ##   loss of optimization opportunities to consider.
    ## * raising an exception is usually (a lot) slower than returning an error
    ##   through a Result - at raise time, capturing a call stack and allocating
    ##   memory for the Exception is expensive, so the performance difference
    ##   comes down to the complexity of the error type used.
    ## * checking for errors with Result is local branching operation that also
    ##   happens on the happy path - this may be a cost.
    ##
    ## An accurate summary might be that Exceptions are at its most efficient
    ## when errors are not handled and don't happen.
    ##
    ## # Relevant nim bugs
    ##
    ## https://github.com/nim-lang/Nim/issues/13799 - type issues
    ## https://github.com/nim-lang/Nim/issues/8745 - genericReset slow
    ## https://github.com/nim-lang/Nim/issues/13879 - double-zero-init slow
    ## https://github.com/nim-lang/Nim/issues/14318 - generic error raises pragma

    case o: bool
    of false:
      e: E
    of true:
      v: T

func raiseResultError[T, E](self: Result[T, E]) {.noreturn, noinline.} =
  # noinline because raising should take as little space as possible at call
  # site
  mixin toException

  when E is ref Exception:
    if self.e.isNil: # for example Result.default()!
      raise (ref ResultError[void])(msg: "Trying to access value with err (nil)")
    raise self.e
  elif compiles(toException(self.e)):
    raise toException(self.e)
  elif compiles($self.e):
    raise (ref ResultError[E])(
      error: self.e, msg: "Trying to access value with err: " & $self.e)
  else:
    raise (res ResultError[E])(msg: "Trying to access value with err", error: self.e)

func raiseResultDefect(m: string, v: auto) {.noreturn, noinline.} =
  mixin `$`
  when compiles($v): raise (ref ResultDefect)(msg: m & ": " & $v)
  else: raise (ref ResultDefect)(msg: m)

func raiseResultDefect(m: string) {.noreturn, noinline.} =
  raise (ref ResultDefect)(msg: m)

template assertOk(self: Result) =
  if not self.o:
    when self.E isnot void:
      raiseResultDefect("Trying to acces value with err Result", self.e)
    else:
      raiseResultDefect("Trying to acces value with err Result")

template ok*[T, E](R: type Result[T, E], x: auto): R =
  ## Initialize a result with a success and value
  ## Example: `Result[int, string].ok(42)`
  R(o: true, v: x)

template ok*[T, E](self: var Result[T, E], x: auto) =
  ## Set the result to success and update value
  ## Example: `result.ok(42)`
  self = ok(type self, x)

template err*[T, E](R: type Result[T, E], x: auto): R =
  ## Initialize the result to an error
  ## Example: `Result[int, string].err("uh-oh")`
  R(o: false, e: x)

template err*[T](R: type Result[T, void]): R =
  R(o: false)

template err*[T, E](self: var Result[T, E], x: auto) =
  ## Set the result as an error
  ## Example: `result.err("uh-oh")`
  self = err(type self, x)

template err*[T](self: var Result[T, void]) =
  ## Set the result as an error
  ## Example: `result.err()`
  self = err(type self)

template ok*(v: auto): auto = ok(typeof(result), v)
template err*(v: auto): auto = err(typeof(result), v)

template isOk*(self: Result): bool = self.o
template isErr*(self: Result): bool = not self.o

func map*[T, E, A](
    self: Result[T, E], f: proc(x: T): A): Result[A, E] {.inline.} =
  ## Transform value using f, or return error
  ##
  ## ```
  ## let r = Result[int, cstring).ok(42)
  ## assert r.map(proc (v: int): int = $v).get() == "42"
  ## ```
  if self.o: result.ok(f(self.v))
  else: result.err(self.e)

func flatMap*[T, E, A](
    self: Result[T, E], f: proc(x: T): Result[A, E]): Result[A, E] {.inline.} =
  if self.o: f(self.v)
  else: Result[A, E].err(self.e)

func mapErr*[T: not void, E, A](
    self: Result[T, E], f: proc(x: E): A): Result[T, A] {.inline.} =
  ## Transform error using f, or return value
  if self.o: result.ok(self.v)
  else: result.err(f(self.e))

func mapConvert*[T0, E0](
    self: Result[T0, E0], T1: type): Result[T1, E0] {.inline.} =
  ## Convert result value to A using an conversion
  # Would be nice if it was automatic...
  if self.o: result.ok(T1(self.v))
  else: result.err(self.e)

func mapCast*[T0, E0](
    self: Result[T0, E0], T1: type): Result[T1, E0] {.inline.} =
  ## Convert result value to A using a cast
  ## Would be nice with nicer syntax...
  if self.o: result.ok(cast[T1](self.v))
  else: result.err(self.e)

template `and`*[T0, E, T1](self: Result[T0, E], other: Result[T1, E]): Result[T1, E] =
  ## Evaluate `other` iff self.isOk, else return error
  ## fail-fast - will not evaluate other if a is an error
  let s = self
  if s.o:
    other
  else:
    when type(self) is type(other):
      s
  else:
    type R = type(other)
      err(R, s.e)

template `or`*[T, E0, E1](self: Result[T, E0], other: Result[T, E1]): Result[T, E1] =
  ## Evaluate `other` iff `not self.isOk`, else return `self`
  ## fail-fast - will not evaluate `other` if `self` is ok
  ##
  ## ```
  ## func f(): Result[int, SomeEnum] =
  ##   f2() or err(EnumValue) # Collapse errors from other module / function
  ## ```
  let s = self
  if s.o:
    when type(self) is type(other):
      s
    else:
      type R = type(other)
      ok(R, s.v)
  else:
    other

template catch*(body: typed): Result[type(body), ref CatchableError] =
  ## Catch exceptions for body and store them in the Result
  ##
  ## ```
  ## let r = catch: someFuncThatMayRaise()
  ## ```
  type R = Result[type(body), ref CatchableError]

  try:
    R.ok(body)
  except CatchableError as e:
    R.err(e)

template capture*[E: Exception](T: type, someExceptionExpr: ref E): Result[T, ref E] =
  ## Evaluate someExceptionExpr and put the exception into a result, making sure
  ## to capture a call stack at the capture site:
  ##
  ## ```
  ## let e: Result[void, ValueError] = void.capture((ref ValueError)(msg: "test"))
  ## echo e.error().getStackTrace()
  ## ```
  type R = Result[T, ref E]

  var ret: R
  try:
    # TODO is this needed? I think so, in order to grab a call stack, but
    #      haven't actually tested...
    if true:
      # I'm sure there's a nicer way - this just works :)
      raise someExceptionExpr
  except E as caught:
    ret = R.err(caught)
  ret

func `==`*[T0, E0, T1, E1](lhs: Result[T0, E0], rhs: Result[T1, E1]): bool {.inline.} =
  if lhs.o != rhs.o:
    false
  elif lhs.o: # and rhs.o implied
    lhs.v == rhs.v
  else:
    lhs.e == rhs.e

func get*[T: not void, E](self: Result[T, E]): T {.inline.} =
  ## Fetch value of result if set, or raise Defect
  ## Exception bridge mode: raise given Exception instead
  ## See also: Option.get
  assertOk(self)
  self.v

func tryGet*[T: not void, E](self: Result[T, E]): T {.inline.} =
  ## Fetch value of result if set, or raise
  ## When E is an Exception, raise that exception - otherwise, raise a ResultError[E]
  mixin raiseResultError
  if not self.o: self.raiseResultError()
  self.v

func get*[T, E](self: Result[T, E], otherwise: T): T {.inline.} =
  ## Fetch value of result if set, or return the value `otherwise`
  ## See `valueOr` for a template version that avoids evaluating `otherwise`
  ## unless necessary
  if self.o: self.v
  else: otherwise

func get*[T, E](self: var Result[T, E]): var T {.inline.} =
  ## Fetch value of result if set, or raise Defect
  ## Exception bridge mode: raise given Exception instead
  ## See also: Option.get
  assertOk(self)
  self.v

template `[]`*[T: not void, E](self: Result[T, E]): T =
  ## Fetch value of result if set, or raise Defect
  ## Exception bridge mode: raise given Exception instead
  mixin get
  self.get()

template `[]`*[T, E](self: var Result[T, E]): var T =
  ## Fetch value of result if set, or raise Defect
  ## Exception bridge mode: raise given Exception instead
  mixin get
  self.get()

template unsafeGet*[T, E](self: Result[T, E]): T =
  ## Fetch value of result if set, undefined behavior if unset
  ## See also: Option.unsafeGet
  assert self.o

  self.v

func expect*[T: not void, E](self: Result[T, E], m: string): T =
  ## Return value of Result, or raise a `Defect` with the given message - use
  ## this helper to extract the value when an error is not expected, for example
  ## because the program logic dictates that the operation should never fail
  ##
  ## ```nim
  ## let r = Result[int, int].ok(42)
  ## # Put here a helpful comment why you think this won't fail
  ## echo r.expect("r was just set to ok(42)")
  ## ```
  if not self.o:
    raiseResultDefect(m, self.e)
  self.v

func expect*[T: not void, E](self: var Result[T, E], m: string): var T =
  if not self.o:
    raiseResultDefect(m, self.e)
  self.v

func `$`*(self: Result): string =
  ## Returns string representation of `self`
  if self.o: "Ok(" & $self.v & ")"
  else: "Err(" & $self.e & ")"

func error*[T, E](self: Result[T, E]): E =
  ## Fetch error of result if set, or raise Defect
  if self.o:
    when T is not void:
      raiseResultDefect("Trying to access error when value is set", self.v)
    else:
      raise (ref ResultDefect)(msg: "Trying to access error when value is set")
  self.e

template value*[T, E](self: Result[T, E]): T =
  mixin get
  self.get()

template value*[T, E](self: var Result[T, E]): T =
  mixin get
  self.get()

template valueOr*[T, E](self: Result[T, E], def: T): T =
  ## Fetch value of result if set, or supplied default
  ## default will not be evaluated iff value is set
  if self.o: self.v
  else: def

# void support

template ok*[E](R: type Result[void, E]): auto =
  ## Initialize a result with a success and value
  ## Example: `Result[int, string].ok(42)`
  R(o: true)

template ok*[E](self: var Result[void, E]) =
  ## Set the result to success and update value
  ## Example: `result.ok(42)`
  mixin ok
  self = (type self).ok()

template ok*(): auto =
  mixin ok
  ok(typeof(result))

template err*(): auto =
  mixin err
  err(typeof(result))

# TODO:
# Supporting `map` and `get` operations on a `void` result is quite
# an unusual API. We should provide some motivating examples.

func map*[E, A](
    self: Result[void, E], f: proc(): A): Result[A, E] {.inline.} =
  ## Transform value using f, or return error
  if self.o: result.ok(f())
  else: result.err(self.e)

func flatMap*[E, A](
    self: Result[void, E], f: proc(): Result[A, E]): Result[A, E] {.inline.} =
  if self.o: f(self.v)
  else: Result[A, E].err(self.e)

func mapErr*[E, A](
    self: Result[void, E], f: proc(x: E): A): Result[void, A] {.inline.} =
  ## Transform error using f, or return value
  if self.o: result.ok()
  else: result.err(f(self.e))

func map*[T, E](
    self: Result[T, E], f: proc(x: T)): Result[void, E] {.inline.} =
  ## Transform value using f, or return error
  if self.o: f(self.v); result.ok()
  else: result.err(self.e)

func get*[E](self: Result[void, E]) {.inline.} =
  ## Fetch value of result if set, or raise
  ## See also: Option.get
  mixin assertOk
  assertOk(self)

func tryGet*[E](self: Result[void, E]) {.inline.} =
  ## Fetch value of result if set, or raise a CatchableError
  mixin raiseResultError
  if not self.o:
    self.raiseResultError()

template `[]`*[E](self: Result[void, E]) =
  ## Fetch value of result if set, or raise
  mixin get
  self.get()

template unsafeGet*[E](self: Result[void, E]) =
  ## Fetch value of result if set, undefined behavior if unset
  ## See also: Option.unsafeGet
  assert self.o

func expect*[E](self: Result[void, E], msg: string) =
  if not self.o:
    raise (ref ResultDefect)(msg: msg)

func `$`*[E](self: Result[void, E]): string =
  ## Returns string representation of `self`
  if self.o: "Ok()"
  else: "Err(" & $self.e & ")"

template value*[E](self: Result[void, E]) =
  mixin get
  self.get()

template value*[E](self: var Result[void, E]) =
  mixin get
  self.get()

template `?`*[T, E](self: Result[T, E]): T =
  ## Early return - if self is an error, we will return from the current
  ## function, else we'll move on..
  ##
  ## ```
  ## let v = ? funcWithResult()
  ## echo v # prints value, not Result!
  ## ```
  ## Experimental
  # TODO the v copy is here to prevent multiple evaluations of self - could
  #      probably avoid it with some fancy macro magic..
  let v = (self)
  if not v.o:
    when typeof(result) is typeof(v):
      return v
    else:
      return err(typeof(result), v.e)

  v.v

when isMainModule:
  type R = Result[int, string]

  # Basic usage, producer
  func works(): R = R.ok(42)
  func works2(): R = result.ok(42)
  func fails(): R = R.err("dummy")
  func fails2(): R = result.err("dummy")

  func raises(): int =
    raise (ref CatchableError)(msg: "hello")

  # Basic usage, consumer
  let
    rOk = works()
    rOk2 = works2()
    rErr = fails()
    rErr2 = fails2()

  doAssert rOk.isOk
  doAssert rOk2.isOk
  doAssert rOk.get() == 42
  doAssert (not rOk.isErr)
  doAssert rErr.isErr
  doAssert rErr2.isErr

  # Combine
  doAssert (rOk and rErr).isErr
  doAssert (rErr and rOk).isErr
  doAssert (rOk or rErr).isOk
  doAssert (rErr or rOk).isOk

  # `and` heterogenous types
  doAssert (rOk and rOk.map(proc(x: auto): auto = $x))[] == $(rOk[])

# `or` heterogenous types
doAssert (rErr or rErr.mapErr(proc(x: auto): auto = len(x))).error == len(rErr.error)

  # Exception on access
  let va = try: discard rOk.error; false except: true
  doAssert va, "not an error, should raise"

  # Exception on access
  let vb = try: discard rErr.value; false except: true
  doAssert vb, "not an value, should raise"

  var x = rOk

  # Mutate
  x.err("failed now")

  doAssert x.isErr

  # Exceptions -> results
  let c = catch:
    raises()

  doAssert c.isErr

  # De-reference
  try:
    echo rErr[]
    doAssert false
  except:
    discard

  doAssert rOk.valueOr(50) == rOk.value
  doAssert rErr.valueOr(50) == 50

  # Comparisons
  doAssert (works() == works2())
  doAssert (fails() == fails2())
  doAssert (works() != fails())

  var counter = 0
  proc incCounter(): R =
    counter += 1
    R.ok(counter)

  doAssert (rErr and incCounter()).isErr, "b fails"
  doAssert counter == 0, "should fail fast on rErr"

  # Mapping
  doAssert (rOk.map(func(x: int): string = $x)[] == $rOk.value)
  doAssert (rOk.flatMap(
    proc(x: int): Result[string, string] = Result[string, string].ok($x))[] == $rOk.value)
  doAssert (rErr.mapErr(func(x: string): string = x & "no!").error == (rErr.error & "no!"))

  # Exception interop
  let e = capture(int, (ref ValueError)(msg: "test"))
  doAssert e.isErr
  doAssert e.error.msg == "test"

  try:
    discard e.tryGet
    doAssert false, "should have raised"
  except ValueError as e:
    doAssert e.msg == "test"

  # Nice way to checks
  if (let v = works(); v.isOk):
    doAssert v[] == v.value

  # Can formalise it into a template (https://github.com/arnetheduck/nim-result/issues/8)
  template `?=`*(v: untyped{nkIdent}, vv: Result): bool =
    (let vr = vv; template v: auto {.used.} = unsafeGet(vr); vr.isOk)
  if f ?= works():
    doAssert f == works().value

  doAssert $rOk == "Ok(42)"

  doAssert rOk.mapConvert(int64)[] == int64(42)
  doAssert rOk.mapCast(int8)[] == int8(42)
  doAssert rOk.mapConvert(uint64)[] == uint64(42)

  try:
    discard rErr.get()
    doAssert false
  except Defect: # TODO catching defects is undefined behaviour, use external test suite?
    discard

  try:
    discard rOk.error()
    doAssert false
  except Defect: # TODO catching defects is undefined behaviour, use external test suite?
    discard

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
  doAssert (rOk + rOk)[] == rOk.value + rOk.value

  iterator items[T, E](self: Result[T, E]): T =
    ## Iterate over result as if it were a collection of either 0 or 1 items
    ## TODO should a Result[seq[X]] iterate over items in seq? there are
    ##      arguments for and against
    if self.isOk:
      yield self.value

  # Iteration
  var counter2 = 0
  for v in rOk:
    counter2 += 1

  doAssert counter2 == 1, "one-item collection when set"

  func testOk(): Result[int, string] =
    ok 42

  func testErr(): Result[int, string] =
    err "323"

  doAssert testOk()[] == 42
  doAssert testErr().error == "323"

  doAssert testOk().expect("testOk never fails") == 42

  func testQn(): Result[int, string] =
    let x = ?works() - ?works()
    result.ok(x)

  func testQn2(): Result[int, string] =
    # looks like we can even use it creatively like this
    if ?fails() == 42: raise (ref ValueError)(msg: "shouldn't happen")

  func testQn3(): Result[bool, string] =
    # different T but same E
    let x = ?works() - ?works()
    result.ok(x == 0)

  doAssert testQn()[] == 0
  doAssert testQn2().isErr
  doAssert testQn3()[]

proc heterOr(): Result[int, int] =
  let value = ? (rErr or err(42))  # TODO ? binds more tightly than `or` - can that be fixed?
  doAssert value + 1 == value, "won't reach, ? will shortcut execution"
  ok(value)

doAssert heterOr().error() == 42

  type
    AnEnum = enum
      anEnumA
      anEnumB
    AnException = ref object of CatchableError
      v: AnEnum

  func toException(v: AnEnum): AnException = AnException(v: v)

  func testToException(): int =
    try:
      var r = Result[int, AnEnum].err(anEnumA)
    r.tryGet
    except AnException:
      42

  doAssert testToException() == 42

  type
    AnEnum2 = enum
      anEnum2A
      anEnum2B

  func testToString(): int =
    try:
      var r = Result[int, AnEnum2].err(anEnum2A)
      r.tryGet
    except ResultError[AnEnum2]:
      42

  doAssert testToString() == 42

  type VoidRes = Result[void, int]

  func worksVoid(): VoidRes = VoidRes.ok()
  func worksVoid2(): VoidRes = result.ok()
  func failsVoid(): VoidRes = VoidRes.err(42)
  func failsVoid2(): VoidRes = result.err(42)

  let
    vOk = worksVoid()
    vOk2 = worksVoid2()
    vErr = failsVoid()
    vErr2 = failsVoid2()

  doAssert vOk.isOk
  doAssert vOk2.isOk
  doAssert vErr.isErr
  doAssert vErr2.isErr

  vOk.get()
  vOk.expect("should never fail")

  doAssert vOk.map(proc (): int = 42).get() == 42

  rOk.map(proc(x: int) = discard).get()

  try:
    rErr.map(proc(x: int) = discard).get()
    doAssert false
  except:
    discard

  doAssert vErr.mapErr(proc(x: int): int = 10).error() == 10
