# Copyright (c) 2019-2025 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license (license terms in the root directory or at http://opensource.org/licenses/MIT).
#   * Apache v2 license (license terms in the root directory or at http://www.apache.org/licenses/LICENSE-2.0).
# at your option. This file may not be copied, modified, or distributed except according to those terms.

## Compile + runtime coverage of the public API under
## ``{.experimental: "strictCaseObjects".}``. Every call path that
## reaches ``Result``'s variant fields — ``value``, ``error``,
## ``tryValue``, ``tryError``, ``mapConvertErr``, ``mapCastErr``, the
## three ``==`` overloads, and the raise helpers — must compile and
## behave identically under strict to the behaviour documented in the
## core test suite.
##
## The four specialisations ``Result[T, E]`` / ``Result[void, E]`` /
## ``Result[T, void]`` / ``Result[void, void]`` are exercised so the
## ``when`` hoists around the ``case self.oResultPrivate`` dispatch
## instantiate correctly for each shape.

{.experimental: "strictCaseObjects".}
{.used.}

import std/strutils
import ../results

# ---------------------------------------------------------------------------
# Result[int, string] — full accessor and combinator surface on the Ok path.
# ---------------------------------------------------------------------------
block okPath:
  let r = Result[int, string].ok(42)
  doAssert r.isOk and not r.isErr
  doAssert r.value == 42
  doAssert r.get == 42
  doAssert r.tryValue == 42
  doAssert r.valueOr(-1) == 42
  doAssert r.errorOr("default") == "default"
  doAssert ($r).contains("42")
  doAssert r.map(
    proc(x: int): int =
      x + 1
  ).value == 43
  doAssert r.mapErr(
    proc(e: string): int =
      e.len
  ).value == 42
  doAssert r.mapConvert(int64).value == 42'i64
  doAssert r.mapCast(uint).value == 42'u
  doAssert r.mapConvertErr(cstring).value == 42
  doAssert r == Result[int, string].ok(42)
  var seen = 0
  for v in r.values:
    seen = v
  doAssert seen == 42
  # tryError on an Ok value raises ResultError parameterised on T.
  try:
    discard r.tryError
    doAssert false
  except ResultError[int] as e:
    doAssert e.error == 42

# ---------------------------------------------------------------------------
# Result[int, string] — Err path including tryValue's exception bridge and
# cross-overload inequality.
# ---------------------------------------------------------------------------
block errPath:
  let r = Result[int, string].err("boom")
  doAssert r.isErr
  doAssert r.error == "boom"
  doAssert r.tryError == "boom"
  doAssert r.valueOr(-1) == -1
  doAssert r.errorOr("fallback") == "boom"
  doAssert r.mapConvertErr(cstring).error == cstring "boom"
  doAssert r == Result[int, string].err("boom")
  doAssert r != Result[int, string].ok(99)
  try:
    discard r.tryValue
    doAssert false
  except ResultError[string] as e:
    # The error payload is the dispatch-critical assertion; the ``msg``
    # wording may vary across Nim versions depending on which exception-
    # bridge arm fires for ``string``.
    doAssert e.error == "boom"

# ---------------------------------------------------------------------------
# Opt[T] (= Result[T, void]) — Some. Exercises the void-E specialisation
# with a non-void T.
# ---------------------------------------------------------------------------
block someOpt:
  let o = Opt.some(7)
  doAssert o.isOk and o.value == 7
  doAssert o.map(
    proc(x: int): int =
      x * 2
  ).value == 14
  doAssert o == Opt.some(7)
  doAssert o != Opt.none(int)
  # tryError on ``Result[T, void]`` returns ``void`` (no payload to
  # bind) and raises when self is Ok — so the call is a statement, not
  # an expression.
  try:
    o.tryError
    doAssert false
  except ResultError[int] as e:
    doAssert e.error == 7

# ---------------------------------------------------------------------------
# Opt[T] — None. Exercises the void-E specialisation: the raise helper
# must raise without touching a non-existent error field.
# ---------------------------------------------------------------------------
block noneOpt:
  let o = Opt.none(int)
  doAssert o.isErr and o.valueOr(99) == 99
  doAssert o == Opt.none(int)
  try:
    discard o.tryValue
    doAssert false
  except ResultError[void]:
    discard

# ---------------------------------------------------------------------------
# Result[void, string] — Ok. Exercises the void-T specialisation.
# ---------------------------------------------------------------------------
block voidResultOk:
  let r = Result[void, string].ok()
  doAssert r.isOk
  doAssert r == Result[void, string].ok()
  r.tryValue
  # tryError returns ``lent string`` on the non-void-E side, so
  # discard is required.
  try:
    discard r.tryError
    doAssert false
  except ResultError[void]:
    discard

# ---------------------------------------------------------------------------
# Result[void, string] — Err.
# ---------------------------------------------------------------------------
block voidResultErr:
  let r = Result[void, string].err("nope")
  doAssert r.isErr and r.error == "nope"
  doAssert r == Result[void, string].err("nope")
  doAssert r != Result[void, string].ok()

# ---------------------------------------------------------------------------
# Result[void, void] — Err. The fully-void specialisation has no payload
# on either side; tryValue must still raise on access.
# ---------------------------------------------------------------------------
block fullyVoidErr:
  let r = Result[void, void].err()
  try:
    r.tryValue
    doAssert false
  except ResultError[void]:
    discard

# ---------------------------------------------------------------------------
# Exception bridge — E is ``ref Exception``. tryValue should re-raise the
# stored exception directly.
# ---------------------------------------------------------------------------
block refExceptionErr:
  type MyErr = ref object of CatchableError
  let r = Result[int, MyErr].err(MyErr(msg: "hi"))
  try:
    discard r.tryValue
    doAssert false
  except MyErr as e:
    doAssert e.msg == "hi"

# ---------------------------------------------------------------------------
# Exception bridge — E is ``ref Exception`` with a nil payload (e.g. the
# shape produced by ``Result.default()``). tryValue must not dereference
# nil; it should raise a typed ResultError[void] carrying a diagnostic
# that mentions the nil condition.
# ---------------------------------------------------------------------------
block refExceptionNilErr:
  type MyErr = ref object of CatchableError
  let r = Result[int, MyErr].err(MyErr(nil))
  try:
    discard r.tryValue
    doAssert false
  except ResultError[void] as e:
    doAssert "nil" in e.msg

# ---------------------------------------------------------------------------
# Exception bridge — user-defined ``toException(E)`` converter. The
# exception-type shape surfaced by tryValue is Nim-version-dependent
# (open-sym resolution for ``mixin toException``), so the assertion is
# limited to "some CatchableError was raised".
# ---------------------------------------------------------------------------
type AnEnum = enum
  aeOops

func toException(v: AnEnum): ref CatchableError =
  (ref CatchableError)(msg: $v)

block toExceptionArm:
  let r = Result[int, AnEnum].err(aeOops)
  var raised = false
  try:
    discard r.tryValue
  except CatchableError:
    raised = true
  doAssert raised

# ---------------------------------------------------------------------------
# Exception bridge — opaque E (no ``$`` and no ``toException``). Falls
# through to the generic ResultError[E] wrapper.
# ---------------------------------------------------------------------------
block opaqueErr:
  type Opaque = object
    dummy: int

  let r = Result[int, Opaque].err(Opaque(dummy: 5))
  try:
    discard r.tryValue
    doAssert false
  except ResultError[Opaque] as e:
    doAssert e.error.dummy == 5
    doAssert "Trying to access value with err" in e.msg

# ---------------------------------------------------------------------------
# mapCastErr — reinterprets the error payload via ``cast`` rather than a
# conversion, so E0 and E1 must share a runtime representation.
# ---------------------------------------------------------------------------
block mapCastErrOk:
  let r = Result[int, int32].ok(42)
  doAssert r.mapCastErr(uint32).value == 42

block mapCastErrErr:
  let r = Result[int, int32].err(7'i32)
  doAssert int32(r.mapCastErr(uint32).error) == 7

echo "strict_caseobjects: OK"
