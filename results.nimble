# Package

version = "0.5.1"
author = "Jacek Sieka"
description = "Friendly, exception-free value-or-error returns, similar to Option[T]"
license = "MIT"
skipDirs = @["benchmarks", "tests"]
installFiles = @["results.nim"]
# Dependencies
requires "nim >= 1.2"

proc test(env, path: string) =
  # Compilation language is controlled by TEST_LANG
  var lang = "c"
  if existsEnv"TEST_LANG":
    lang = getEnv"TEST_LANG"
  exec "nim " & lang & " " & env & " -r " & path

task test, "Runs the test suite":
  for f in ["test_results.nim", "test_results2.nim"]:
    for opt in ["-d:resultsGenericsOpenSym:false", "-d:resultsGenericsOpenSym:true"]:
      test opt, "tests/" & f
      if (NimMajor, NimMinor) >= (2, 0):
        test opt & " --mm:refc", "tests/" & f
  # strictCaseObjects test — Nim >= 2.0 only (the pragma exists on earlier
  # versions but the flow-analysis diagnostics this test exercises are
  # only stabilised on modern Nim).
  if (NimMajor, NimMinor) >= (2, 0):
    test "", "tests/test_strict_caseobjects.nim"
    test "--mm:refc", "tests/test_strict_caseobjects.nim"

task bench, "Run benchmark":
  test "-d:release", "benchmarks/benchmark.nim"
