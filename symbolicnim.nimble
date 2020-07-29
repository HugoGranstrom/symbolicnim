# Package

version       = "0.1.0"
author        = "HugoGranstrom"
description   = "A symbolic algebra library written in Nim"
license       = "MIT"
srcDir        = "src"

task test, "Runs the test suite":
  const filenames = ["tests/testSuite.nim"]
  echo "Running tests using Refc GC:"
  for name in filenames:
    exec "nim c -r --gc:refc -o:bin/testSuite " & name
  echo "Running tests using GC:ARC:"
  for name in filenames:
    exec "nim c -r --gc:arc -o:bin/testSuite " & name
  echo "Running tests using GC:ORC:"
  for name in filenames:
    exec "nim c -r --gc:orc -o:bin/testSuite " & name

# Dependencies

requires "nim >= 1.3.5"
