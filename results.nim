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
    ## import results
    ##
    ## # Re-export `results` so that API is always available to users of your module!
    ## export results
    ##
    ## # It's convenient to create an alias - most likely, you'll do just fine
    ## # with strings or cstrings as error for a start
    ##
    ## type R = Result[int, string]
    ##
    ## # Once you have a type, use `ok` and `err`:
    ##
    ## func works(): R =
    ##   # ok says it went... ok!
    ##   R.ok 42
    ## func fails(): R =
    ##   # or type it like this, to not repeat the type:
    ##   result.err "bad luck"
    ##
    ## func alsoWorks(): R =
    ##   # or just use the shortcut - auto-deduced from the return type!
    ##   ok(24)
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
    ## # You can use the question mark operator to pass errors up the call stack
    ## func f(): R =
    ##   let x = ?works() - ?fails()
    ##   assert false, "will never reach"
    ##
    ## # If you provide this exception converter, this exception will be raised on
    ## # `tryGet`:
    ## func toException(v: Error): ref CatchableError = (ref CatchableError)(msg: $v)
    ## try:
    ##   RE[int].err(a).tryGet()
    ## except CatchableError:
    ##   echo "in here!"
    ##
    ## # You can use `Opt[T]` as a replacement for `Option` = `Opt` is an alias for
    ## # `Result[T, void]`, meaning you can use the full `Result` API on it:
    ## let x = Opt[int].ok(42)
    ## echo x.get()
    ##
    ## # ... or `Result[void, E]` as a replacement for `bool`, providing extra error
    ## # information!
    ## let y = Result[void, string].err("computation failed")
    ## echo y.error()
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
    ## A natural "error API" progression is starting with `Opt[T]`, then
    ## `Result[T, cstring]`, `Result[T, enum]` and `Result[T, object]` in
    ## escalating order of complexity.
    ##
    ## # Result equivalences with other types
    ##
    ## Result allows tightly controlling the amount of information that a
    ## function gives to the caller:
    ##
    ## ## `Result[void, void] == bool`
    ##
    ## Neither value nor error information, it either worked or didn't. Most
    ## often used for `proc`:s with side effects.
    ##
    ## ## `Result[T, void] == Option[T]`
    ##
    ## Return value if it worked, else tell the caller it failed. Most often
    ## used for simple computiations.
    ##
    ## Works as a fully replacement for `Option[T]` (aliased as `Opt[T]`)
    ##
    ## ## `Result[T, E]` -
    ##
    ## Return value if it worked, or a statically known piece of information
    ## when it didn't - most often used when a function can fail in more than
    ## one way - E is typically a `string` or an `enum`.
    ##
    ## ## `Result[T, ref E]`
    ##
    ## Returning a `ref E` allows introducing dynamically typed error
    ## information, similar to exceptions.
    ##
    ## # Other implemenations in nim
    ##
    ## There are other implementations in nim that you might prefer:
    ## * Either from nimfp: https://github.com/vegansk/nimfp/blob/master/src/fp/either.nim
    ## * result_type: https://github.com/kapralos/result_type/
    ##
    ## `Option` compatibility
    ##
    ## `Result[T, void]` is similar to `Option[T]`, except it can be used with
    ## all `Result` operators and helpers.
    ##
    ## One difference is `Option[ref|ptr T]` which disallows `nil` - `Opt[T]`
    ## allows an "ok" result to hold `nil` - this can be useful when `nil` is
    ## a valid outcome of a function, but increases complexity for the caller.
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

  Opt*[T] = Result[T, void]

func raiseResultOk[T, E](self: Result[T, E]) {.noreturn, noinline.} =
  # noinline because raising should take as little space as possible at call
  # site
  when T is void:
    raise (ref ResultError[void])(msg: "Trying to access error with value")
  else:
    raise (ref ResultError[T])(msg: "Trying to access error with value", error: self.v)

func raiseResultError[T, E](self: Result[T, E]) {.noreturn, noinline.} =
  # noinline because raising should take as little space as possible at call
  # site
  mixin toException

  when E is ref Exception:
    if self.e.isNil: # for example Result.default()!
      raise (ref ResultError[void])(msg: "Trying to access value with err (nil)")
    raise self.e
  elif E is void:
    raise (ref ResultError[void])(msg: "Trying to access value with err")
  elif compiles(toException(self.e)):
    raise toException(self.e)
  elif compiles($self.e):
    raise (ref ResultError[E])(
      error: self.e, msg: $self.e)
  else:
    raise (ref ResultError[E])(msg: "Trying to access value with err", error: self.e)

func raiseResultDefect(m: string, v: auto) {.noreturn, noinline.} =
  mixin `$`
  when compiles($v): raise (ref ResultDefect)(msg: m & ": " & $v)
  else: raise (ref ResultDefect)(msg: m)

func raiseResultDefect(m: string) {.noreturn, noinline.} =
  raise (ref ResultDefect)(msg: m)

template assertOk(self: Result) =
  # Careful - `self` evaluated multiple times, which is fine in all current uses
  if not self.o:
    when self.E isnot void:
      raiseResultDefect("Trying to access value with err Result", self.e)
    else:
      raiseResultDefect("Trying to access value with err Result")

template ok*[T, E](R: type Result[T, E], x: untyped): R =
  ## Initialize a result with a success and value
  ## Example: `Result[int, string].ok(42)`
  R(o: true, v: x)

template ok*[E](R: type Result[void, E]): R =
  ## Initialize a result with a success and value
  ## Example: `Result[void, string].ok()`
  R(o: true)

template ok*[T: not void, E](self: var Result[T, E], x: untyped) =
  ## Set the result to success and update value
  ## Example: `result.ok(42)`
  self = ok(type self, x)

template ok*[E](self: var Result[void, E]) =
  ## Set the result to success and update value
  ## Example: `result.ok()`
  self = (type self).ok()

template err*[T, E](R: type Result[T, E], x: untyped): R =
  ## Initialize the result to an error
  ## Example: `Result[int, string].err("uh-oh")`
  R(o: false, e: x)

template err*[T](R: type Result[T, cstring], x: string): R =
  ## Initialize the result to an error
  ## Example: `Result[int, string].err("uh-oh")`
  const s = x # avoid dangling cstring pointers
  R(o: false, e: cstring(s))

template err*[T](R: type Result[T, void]): R =
  ## Initialize the result to an error
  ## Example: `Result[int, void].err()`
  R(o: false)

template err*[T, E](self: var Result[T, E], x: untyped) =
  ## Set the result as an error
  ## Example: `result.err("uh-oh")`
  self = err(type self, x)

template err*[T](self: var Result[T, cstring], x: string) =
  const s = x # Make sure we don't return a dangling pointer
  self = err(type self, cstring(s))

template err*[T](self: var Result[T, void]) =
  ## Set the result as an error
  ## Example: `result.err()`
  self = err(type self)

template ok*(v: auto): auto = ok(typeof(result), v)
template ok*(): auto = ok(typeof(result))

template err*(v: auto): auto = err(typeof(result), v)
template err*(): auto = err(typeof(result))

template isOk*(self: Result): bool = self.o
template isErr*(self: Result): bool = not self.o

func map*[T0, E, T1](
    self: Result[T0, E], f: proc(x: T0): T1): Result[T1, E] {.inline.} =
  ## Transform value using f, or return error
  ##
  ## ```
  ## let r = Result[int, cstring).ok(42)
  ## assert r.map(proc (v: int): int = $v).get() == "42"
  ## ```
  if self.o:
    result.ok(f(self.v))
  else:
    when E is void:
      result.err()
    else:
      result.err(self.e)

func map*[T, E](
    self: Result[T, E], f: proc(x: T)): Result[void, E] {.inline.} =
  ## Transform value using f, or return error
  ##
  ## ```
  ## let r = Result[int, cstring).ok(42)
  ## assert r.map(proc (v: int): int = $v).get() == "42"
  ## ```
  if self.o:
    f(self.v)
    result.ok()
  else:
    when E is void:
      result.err()
    else:
      result.err(self.e)

func map*[E, T1](
    self: Result[void, E], f: proc(): T1): Result[T1, E] {.inline.} =
  ## Transform value using f, or return error
  if self.o:
    result.ok(f())
  else:
    when E is void:
      result.err()
    else:
      result.err(self.e)

func map*[E](
    self: Result[void, E], f: proc()): Result[void, E] {.inline.} =
  ## Call f if value is
  if self.o:
    f()
    result.ok()
  else:
    when E is void:
      result.err()
    else:
      result.err(self.e)

func flatMap*[T0, E, T1](
    self: Result[T0, E], f: proc(x: T0): Result[T1, E]): Result[T1, E] {.inline.} =
  if self.o: f(self.v)
  else:
    when E is void:
      Result[T1, void].err()
    else:
      Result[T1, E].err(self.e)

func flatMap*[E, T1](
    self: Result[void, E], f: proc(): Result[T1, E]): Result[T1, E] {.inline.} =
  if self.o: f()
  else:
    when E is void:
      Result[T1, void].err()
    else:
      Result[T1, E].err(self.e)

func mapErr*[T, E0, E1](
    self: Result[T, E0], f: proc(x: E0): E1): Result[T, E1] {.inline.} =
  ## Transform error using f, or leave untouched
  if self.o:
    when T is void:
      result.ok()
    else:
      result.ok(self.v)
  else:
    result.err(f(self.e))

func mapErr*[T, E1](
    self: Result[T, void], f: proc(): E1): Result[T, E1] {.inline.} =
  ## Transform error using f, or return value
  if self.o:
    when T is void:
      result.ok()
    else:
      result.ok(self.v)
  else:
    result.err(f())

func mapErr*[T, E0](
    self: Result[T, E0], f: proc(x: E0)): Result[T, void] {.inline.} =
  ## Transform error using f, or return value
  if self.o:
    when T is void:
      result.ok()
    else:
      result.ok(self.v)
  else:
    f(self.e)
    result.err()

func mapErr*[T](
    self: Result[T, void], f: proc()): Result[T, void] {.inline.} =
  ## Transform error using f, or return value
  if self.o:
    when T is void:
      result.ok()
    else:
      result.ok(self.v)
  else:
    f()
    result.err()

func mapConvert*[T0, E](
    self: Result[T0, E], T1: type): Result[T1, E] {.inline.} =
  ## Convert result value to A using an conversion
  # Would be nice if it was automatic...
  if self.o:
    when T1 is void:
      result.ok()
    else:
      result.ok(T1(self.v))
  else:
    when E is void:
      result.err()
    else:
      result.err(self.e)

func mapCast*[T0, E](
    self: Result[T0, E], T1: type): Result[T1, E] {.inline.} =
  ## Convert result value to A using a cast
  ## Would be nice with nicer syntax...
  if self.o: result.ok(cast[T1](self.v))
  else:
    when E is void:
      result.err()
    else:
      result.err(self.e)

template `and`*[T0, E, T1](self: Result[T0, E], other: Result[T1, E]): Result[T1, E] =
  ## Evaluate `other` iff self.isOk, else return error
  ## fail-fast - will not evaluate other if a is an error
  let s = (self) # TODO avoid copy
  if s.o:
    other
  else:
    when type(self) is type(other):
      s
    else:
      type R = type(other)
      when E is void:
        err(R)
      else:
        err(R, s.e)

template `or`*[T, E0, E1](self: Result[T, E0], other: Result[T, E1]): Result[T, E1] =
  ## Evaluate `other` iff `not self.isOk`, else return `self`
  ## fail-fast - will not evaluate `other` if `self` is ok
  ##
  ## ```
  ## func f(): Result[int, SomeEnum] =
  ##   f2() or err(SomeEnum.V) # Collapse errors from other module / function
  ## ```
  let s = (self) # TODO avoid copy
  if s.o:
    when type(self) is type(other):
      s
    else:
      type R = type(other)
      when T is void:
        ok(R)
      else:
        ok(R, s.v)
  else:
    other

template orErr*[T, E0, E1](self: Result[T, E0], error: E1): Result[T, E1] =
  ## Evaluate `other` iff `not self.isOk`, else return `self`
  ## fail-fast - will not evaluate `error` if `self` is ok
  ##
  ## ```
  ## func f(): Result[int, SomeEnum] =
  ##   f2().orErr(SomeEnum.V) # Collapse errors from other module / function
  ## ```
  ##
  ## ** Experimental, may be removed **
  let  s = (self) # TODO avoid copy
  type R = Result[T, E1]
  if s.o:
    when type(self) is R:
      s
    else:
      when T is void:
        ok(R)
      else:
        ok(R, s.v)
  else:
    err(R, error)


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

func `==`*[
    T0: not void, E0: not void,
    T1: not void, E1: not void](
      lhs: Result[T0, E0], rhs: Result[T1, E1]): bool {.inline.} =
  if lhs.o != rhs.o:
    false
  elif lhs.o: # and rhs.o implied
    lhs.v == rhs.v
  else:
    lhs.e == rhs.e

func `==`*[E0, E1](
    lhs: Result[void, E0], rhs: Result[void, E1]): bool {.inline.} =
  if lhs.o != rhs.o:
    false
  elif lhs.o: # and rhs.o implied
    true
  else:
    lhs.e == rhs.e

func `==`*[T0, T1](
    lhs: Result[T0, void], rhs: Result[T1, void]): bool {.inline.} =
  if lhs.o != rhs.o:
    false
  elif lhs.o: # and rhs.o implied
    lhs.v == rhs.v
  else:
    true

func get*[T, E](self: Result[T, E]): T {.inline.} =
  ## Fetch value of result if set, or raise Defect
  ## Exception bridge mode: raise given Exception instead
  ## See also: Option.get
  assertOk(self)
  when T isnot void:
    self.v

func tryGet*[T, E](self: Result[T, E]): T {.inline.} =
  ## Fetch value of result if set, or raise
  ## When E is an Exception, raise that exception - otherwise, raise a ResultError[E]
  mixin raiseResultError
  if not self.o: self.raiseResultError()
  when T isnot void:
    self.v

func get*[T, E](self: Result[T, E], otherwise: T): T {.inline.} =
  ## Fetch value of result if set, or return the value `otherwise`
  ## See `valueOr` for a template version that avoids evaluating `otherwise`
  ## unless necessary
  if self.o: self.v
  else: otherwise

func get*[T: not void, E](self: var Result[T, E]): var T {.inline.} =
  ## Fetch value of result if set, or raise Defect
  ## Exception bridge mode: raise given Exception instead
  ## See also: Option.get
  assertOk(self)
  self.v

template `[]`*[T: not void, E](self: Result[T, E]): T =
  ## Fetch value of result if set, or raise Defect
  ## Exception bridge mode: raise given Exception instead
  self.get()

template `[]`*[E](self: Result[void, E]) =
  ## Fetch value of result if set, or raise Defect
  ## Exception bridge mode: raise given Exception instead
  self.get()

template `[]`*[T: not void, E](self: var Result[T, E]): var T =
  ## Fetch value of result if set, or raise Defect
  ## Exception bridge mode: raise given Exception instead
  self.get()

template unsafeGet*[T: not void, E](self: Result[T, E]): T =
  ## Fetch value of result if set, undefined behavior if unset
  ## See also: `unsafeError`
  self.v

template unsafeGet*[E](self: Result[void, E]) =
  ## Fetch value of result if set, undefined behavior if unset
  ## See also: `unsafeError`
  assert self.o

func expect*[T, E](self: Result[T, E], m: string): T =
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
    when E isnot void:
      raiseResultDefect(m, self.e)
    else:
      raiseResultDefect(m)
  when T isnot void:
    self.v

func expect*[T: not void, E](self: var Result[T, E], m: string): var T =
  if not self.o:
    when E isnot void:
      raiseResultDefect(m, self.e)
    else:
      raiseResultDefect(m)
  self.v

func `$`*[T, E](self: Result[T, E]): string =
  ## Returns string representation of `self`
  if self.o:
    when T is void: "ok()"
    else: "ok(" & $self.v & ")"
  else:
    when E is void: "err()"
    else: "err(" & $self.e & ")"

func error*[T, E](self: Result[T, E]): E =
  ## Fetch error of result if set, or raise Defect
  if self.o:
    when T isnot void:
      raiseResultDefect("Trying to access error when value is set", self.v)
    else:
      raiseResultDefect("Trying to access error when value is set")
  when E isnot void:
    self.e

func tryError*[T, E](self: Result[T, E]): E {.inline.} =
  ## Fetch error of result if set, or raise
  ## Raises a ResultError[T]
  mixin raiseResultOk
  if self.o: self.raiseResultOk()
  when E isnot void:
    self.e

template unsafeError*[T, E: not void](self: Result[T, E]): E =
  ## Fetch value of result if set, undefined behavior if unset
  ## See also: `unsafeGet`
  self.e

template unsafeError*[T](self: Result[T, void]) =
  ## Fetch value of result if set, undefined behavior if unset
  ## See also: `unsafeGet`
  assert not self.o # Emulate field access defect in debug builds

# Alternative spellings for get
template value*[T, E](self: Result[T, E]): T = self.get()
template value*[T: not void, E](self: var Result[T, E]): var T = self.get()

template valueOr*[T: not void, E](self: Result[T, E], def: untyped): T =
  ## Fetch value of result if set, or evaluate `def`
  ## `def` is evaluated lazily, and must be an expression of `T` or exit
  ## the scope (for example using `return` / `raise`)
  ##
  ## Example:
  ## ```
  ## let
  ##   v = Result[int, string].err("hello")
  ##   x = v.valueOr: 42 # x == 42 now
  ##   y = v.valueOr: raise (ref ValueError)(msg: "v is an error, gasp!")
  ## ```
  let s = (self) # TODO avoid copy
  if s.o: s.v
  else:
    when E isnot void:
      template error: E {.used, inject.} = s.e
    def

template errorOr*[T, E: not void](self: Result[T, E], def: untyped): E =
  ## Fetch error of result if not set, or evaluate `def`
  ## `def` is evaluated lazily, and must be an expression of `T` or exit
  ## the scope (for example using `return` / `raise`)
  let s = (self) # TODO avoid copy
  if not s.o: s.e
  else:
    when T isnot void:
      template value: T {.used, inject.} = s.v
    def

func flatten*[T, E](self: Result[Result[T, E], E]): Result[T, E] =
  ## Remove one level of nesting
  if self.o:
    self.v
  else:
    when E is void:
      err(Result[T, E])
    else:
      err(Result[T, E], self.error)

func filter*[T, E](
    self: Result[T, E],
    callback: proc(x: T): Result[void, E]): Result[T, E] =
  ## Apply `callback` to the `self`, iff `self` is not an error. If `callback`
  ## returns an error, return that error, else return `self`

  if self.o:
    callback(self.v) and self
  else:
    self

func filter*[E](
    self: Result[void, E],
    callback: proc(): Result[void, E]): Result[void, E] =
  ## Apply `callback` to the `self`, iff `self` is not an error. If `callback`
  ## returns an error, return that error, else return `self`

  if self.o:
    callback() and self
  else:
    self

func filter*[T](
    self: Result[T, void],
    callback: proc(x: T): bool): Result[T, void] =
  ## Apply `callback` to the `self`, iff `self` is not an error. If `callback`
  ## returns an error, return that error, else return `self`

  if self.o:
    if callback(self.v):
      self
    else:
      Result[T, void].err()
  else:
    self

# Options compatibility

template some*[T](O: type Opt, v: T): Opt[T] =
  ## Create an `Opt` set to a value
  ##
  ## ```
  ## let o = Opt.some(42)
  ## assert o.isSome and o.get() == 42
  ## ```
  Opt[T].ok(v)

template none*(O: type Opt, T: type): Opt[T] =
  ## Create an `Opt` set to none
  ##
  ## ```
  ## let o = Opt.none(int)
  ## assert o.isNone
  ## ```
  Opt[T].err()

template isSome*(o: Opt): bool =
  ## Alias for `isOk`
  isOk o

template isNone*(o: Opt): bool =
  ## Alias of `isErr`
  isErr o

# Syntactic convenience

template `?`*[T, E](self: Result[T, E]): auto =
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
      when E is void:
        return err(typeof(result))
      else:
        return err(typeof(result), v.e)

  when not(T is void):
    v.v

when isMainModule:
  type R = Result[int, string]

  # Basic usage, producer

  block:
    func works(): R = R.ok(42)
    func works2(): R = result.ok(42)
    func works3(): R = ok(42)

    func fails(): R = R.err("dummy")
    func fails2(): R = result.err("dummy")
    func fails3(): R = err("dummy")

    let
      rOk = works()
      rOk2 = works2()
      rOk3 = works3()

      rErr = fails()
      rErr2 = fails2()
      rErr3 = fails3()

    doAssert rOk.isOk
    doAssert rOk2.isOk
    doAssert rOk3.isOk
    doAssert (not rOk.isErr)

    doAssert rErr.isErr
    doAssert rErr2.isErr
    doAssert rErr3.isErr

    # Mutate
    var x = rOk
    x.err("failed now")
    doAssert x.isErr
    doAssert x.error == "failed now"

    # Combine
    doAssert (rOk and rErr).isErr
    doAssert (rErr and rOk).isErr
    doAssert (rOk or rErr).isOk
    doAssert (rErr or rOk).isOk

    # Fail fast
    proc failFast(): int = raiseAssert "shouldn't evaluate"
    proc failFastR(): R = raiseAssert "shouldn't evaluate"

    doAssert (rErr and failFastR()).isErr
    doAssert (rOk or failFastR()).isOk

    # `and` heterogenous types
    doAssert (rOk and Result[string, string].ok($rOk.get())).get() == $(rOk[])

    # `or` heterogenous types
    doAssert (rErr or Result[int, int].err(len(rErr.error))).error == len(rErr.error)

    # Exception on access
    doAssert (try: (discard rOk.tryError(); false) except ResultError[int]: true)
    doAssert (try: (discard rErr.tryGet(); false) except ResultError[string]: true)

    # Value access or default
    doAssert rOk.get(100) == rOk.get()
    doAssert rErr.get(100) == 100

    doAssert rOk.get() == rOk.unsafeGet()

    doAssert rOk.valueOr(failFast()) == rOk.value()
    let rErrV = rErr.valueOr:
      error.len
    doAssert rErrV == rErr.error.len()

    let rOkV = rOk.errorOr:
      $value
    doAssert rOkV == $rOk.get()

    # Exceptions -> results
    func raises(): int =
      raise (ref CatchableError)(msg: "hello")

    let c = catch:
      raises()
    doAssert c.isErr

    # De-reference
    try:
      echo rErr[]
      doAssert false
    except:
      discard

    # Comparisons
    doAssert (rOk == rOk)
    doAssert (rErr == rErr)
    doAssert (rOk != rErr)

    # Mapping
    doAssert (rOk.map(func(x: int): string = $x)[] == $rOk.value)
    doAssert (rOk.map(func(x: int) = discard)).isOk()

    doAssert (rOk.flatMap(
      proc(x: int): Result[string, string] = Result[string, string].ok($x))[] == $rOk.value)

    doAssert (rErr.mapErr(func(x: string): string = x & "no!").error == (rErr.error & "no!"))

    # Casts and conversions
    doAssert rOk.mapConvert(int64)[] == int64(42)
    doAssert rOk.mapConvert(uint64)[] == uint64(42)
    doAssert rOk.mapCast(int8)[] == int8(42)

    doAssert (rErr.orErr(32)).error == 32
    doAssert (rOk.orErr(failFast())).get() == rOk.get()

    # string conversion
    doAssert $rOk == "ok(42)"
    doAssert $rErr == "err(dummy)"

    # Exception interop
    let e = capture(int, (ref ValueError)(msg: "test"))
    doAssert e.isErr
    doAssert e.error.msg == "test"

    try:
      discard rOk.tryError()
      doAssert false, "should have raised"
    except ValueError:
      discard

    try:
      discard e.tryGet()
      doAssert false, "should have raised"
    except ValueError as e:
      doAssert e.msg == "test"

    # Nice way to checks
    if (let v = works(); v.isOk):
      doAssert v[] == v.value

    # Expectations
    doAssert rOk.expect("testOk never fails") == 42

    # Question mark operator
    func testQn(): Result[int, string] =
      let x = ?works() - ?works()
      ok(x)

    func testQn2(): Result[int, string] =
      # looks like we can even use it creatively like this
      if ?fails() == 42: raise (ref ValueError)(msg: "shouldn't happen")

    func testQn3(): Result[bool, string] =
      # different T but same E
      let x = ?works() - ?works()
      ok(x == 0)

    doAssert testQn()[] == 0
    doAssert testQn2().isErr
    doAssert testQn3()[]

    proc heterOr(): Result[int, int] =
      let value = ? (rErr or err(42))  # TODO ? binds more tightly than `or` - can that be fixed?
      doAssert value + 1 == value, "won't reach, ? will shortcut execution"
      ok(value)

    doAssert heterOr().error() == 42

    # Flatten
    doAssert Result[R, string].ok(rOk).flatten() == rOk
    doAssert Result[R, string].ok(rErr).flatten() == rErr

    # Filter
    doAssert rOk.filter(proc(x: int): auto = Result[void, string].ok()) == rOk
    doAssert rOk.filter(proc(x: int): auto = Result[void, string].err("filter")).error == "filter"
    doAssert rErr.filter(proc(x: int): auto = Result[void, string].err("filter")) == rErr

  # Exception conversions - toException must not be inside a block
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

  block: # Result[void, E]
    type VoidRes = Result[void, int]

    func worksVoid(): VoidRes = VoidRes.ok()
    func worksVoid2(): VoidRes = result.ok()
    func worksVoid3(): VoidRes = ok()

    func failsVoid(): VoidRes = VoidRes.err(42)
    func failsVoid2(): VoidRes = result.err(42)
    func failsVoid3(): VoidRes = err(42)

    let
      vOk = worksVoid()
      vOk2 = worksVoid2()
      vOk3 = worksVoid3()

      vErr = failsVoid()
      vErr2 = failsVoid2()
      vErr3 = failsVoid3()

    doAssert vOk.isOk
    doAssert vOk2.isOk
    doAssert vOk3.isOk
    doAssert (not vOk.isErr)

    doAssert vErr.isErr
    doAssert vErr2.isErr
    doAssert vErr3.isErr

    vOk.get()
    vOk.unsafeGet()
    vOk.expect("should never fail")
    vOk[]

    # Comparisons
    doAssert (vOk == vOk)
    doAssert (vErr == vErr)
    doAssert (vOk != vErr)

    # Mapping
    doAssert vOk.map(proc (): int = 42).get() == 42
    vOk.map(proc () = discard).get()

    vOk.mapErr(proc(x: int): int = 10).get()
    vOk.mapErr(proc(x: int) = discard).get()

    doAssert vErr.mapErr(proc(x: int): int = 10).error() == 10

    # string conversion
    doAssert $vOk == "ok()"
    doAssert $vErr == "err(42)"

    # Question mark operator
    func voidF(): VoidRes =
      ok()

    func voidF2(): Result[int, int] =
      ? voidF()

      ok(42)

    doAssert voidF2().isOk

    # flatten
    doAssert Result[VoidRes, int].ok(vOk).flatten() == vOk
    doAssert Result[VoidRes, int].ok(vErr).flatten() == vErr

    # Filter
    doAssert vOk.filter(proc(): auto = Result[void, int].ok()) == vOk
    doAssert vOk.filter(proc(): auto = Result[void, int].err(100)).error == 100
    doAssert vErr.filter(proc(): auto = Result[void, int].err(100)) == vErr

  block: # Result[T, void] aka `Opt`
    type OptInt = Result[int, void]

    func worksOpt(): OptInt = OptInt.ok(42)
    func worksOpt2(): OptInt = result.ok(42)
    func worksOpt3(): OptInt = ok(42)

    func failsOpt(): OptInt = OptInt.err()
    func failsOpt2(): OptInt = result.err()
    func failsOpt3(): OptInt = err()

    let
      oOk = worksOpt()
      oOk2 = worksOpt2()
      oOk3 = worksOpt3()

      oErr = failsOpt()
      oErr2 = failsOpt2()
      oErr3 = failsOpt3()

    doAssert oOk.isOk
    doAssert oOk2.isOk
    doAssert oOk3.isOk
    doAssert (not oOk.isErr)

    doAssert oErr.isErr
    doAssert oErr2.isErr
    doAssert oErr3.isErr

    # Comparisons
    doAssert (oOk == oOk)
    doAssert (oErr == oErr)
    doAssert (oOk != oErr)

    doAssert oOk.get() == oOk.unsafeGet()
    oErr.error()
    oErr.unsafeError()

    # Mapping
    doAssert oOk.map(proc(x: int): string = $x).get() == $oOk.get()
    oOk.map(proc(x: int) = discard).get()

    doAssert oOk.mapErr(proc(): int = 10).get() == oOk.get()
    doAssert oOk.mapErr(proc() = discard).get() == oOk.get()

    doAssert oErr.mapErr(proc(): int = 10).error() == 10

    # string conversion
    doAssert $oOk == "ok(42)"
    doAssert $oErr == "err()"

    proc optQuestion(): OptInt =
      let v = ? oOk
      ok(v)

    doAssert optQuestion().isOk()

    # Flatten
    doAssert Result[OptInt, void].ok(oOk).flatten() == oOk
    doAssert Result[OptInt, void].ok(oErr).flatten() == oErr

    # Filter
    doAssert oOk.filter(proc(x: int): auto = Result[void, void].ok()) == oOk
    doAssert oOk.filter(proc(x: int): auto = Result[void, void].err()).isErr()
    doAssert oErr.filter(proc(x: int): auto = Result[void, void].err()) == oErr

    doAssert oOk.filter(proc(x: int): bool = true) == oOk
    doAssert oOk.filter(proc(x: int): bool = false).isErr()
    doAssert oErr.filter(proc(x: int): bool = true) == oErr

    doAssert Opt.some(42).get() == 42
    doAssert Opt.none(int).isNone()

  block: # `cstring` dangling reference protection
    type CSRes = Result[void, cstring]

    func cstringF(s: string): CSRes =
      when compiles(err(s)):
        doAssert false

    discard cstringF("test")

  block: # Experiments
    # Can formalise it into a template (https://github.com/arnetheduck/nim-result/issues/8)
    template `?=`(v: untyped{nkIdent}, vv: Result): bool =
      (let vr = vv; template v: auto {.used.} = unsafeGet(vr); vr.isOk)

    if f ?= Result[int, string].ok(42):
      doAssert f == 42

    # TODO there's a bunch of operators that one could lift through magic - this
    #      is mainly an example
    template `+`(self, other: Result): untyped =
      ## Perform `+` on the values of self and other, if both are ok
      type R = type(other)
      if self.isOk:
        if other.isOk:
          R.ok(self.value + other.value)
        else:
          R.err(other.error)
      else:
        R.err(self.error)

    let rOk = Result[int, string].ok(42)
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
