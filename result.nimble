# Package

version       = "0.3.0"
author        = "Jacek Sieka"
description   = "Friendly, exception-free value-or-error returns, similar to Option[T]"
license       = "MIT"
skipDirs      = @["benchmarks"]
installFiles  = @["result.nim", "results.nim"]
# Dependencies

requires "nim >= 1.0.0"

proc test(env, path: string) =
  # Compilation language is controlled by TEST_LANG
  var lang = "c"
  if existsEnv"TEST_LANG":
    lang = getEnv"TEST_LANG"
  exec "nim " & lang & " " & env &
    " -r " & path

task test, "Runs the test suite":
  test "", "results.nim"
