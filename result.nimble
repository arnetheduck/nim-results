# Package

version       = "0.2.0"
author        = "Jacek Sieka"
description   = "Friendly, exception-free value-or-error returns, similar to Option[T]"
license       = "MIT"
skipDirs      = @["benchmarks"]
installFiles  = @["result.nim", "results.nim"]
# Dependencies

requires "nim >= 1.0.0"

task test, "Runs the test suite":
  exec "nim c -r result"
