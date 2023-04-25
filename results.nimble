# Package

version       = "0.4.0"
author        = "Jacek Sieka"
description   = "Friendly, exception-free value-or-error returns, similar to Option[T]"
license       = "MIT"
skipDirs      = @["benchmarks", "tests"]
installFiles  = @["results.nim"]
# Dependencies

requires "nim >= 1.2"

proc test(env, path: string) =
  # Compilation language is controlled by TEST_LANG
  var lang = "c"
  if existsEnv"TEST_LANG":
    lang = getEnv"TEST_LANG"
  exec "nim " & lang & " " & env &
    " -r " & path

task test, "Runs the test suite":
  for f in ["test_results.nim", "test_results2.nim"]:
    test "", "tests/" & f

task bench, "Run benchmark":
  test "-d:release", "benchmarks/benchmark.nim"
