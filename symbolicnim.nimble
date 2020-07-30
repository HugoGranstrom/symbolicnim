# Package

version       = "0.1.1"
author        = "HugoGranstrom"
description   = "A symbolic algebra library written in Nim"
license       = "MIT"
srcDir        = "src"

task test, "Runs the test suite":
  const filenames = ["tests/testSuite.nim"]
  echo "####################\nRunning tests using Refc GC:\n####################"
  for name in filenames:
    exec "nim c -r --gc:refc -o:bin/testSuite " & name
  echo "####################\nRunning tests using GC:ARC:\n####################"
  for name in filenames:
    exec "nim c -r --gc:arc -o:bin/testSuite " & name
  echo "####################\nRunning tests using GC:ORC:\n####################"
  for name in filenames:
    exec "nim c -r --gc:orc -o:bin/testSuite " & name

# Dependencies

requires "nim >= 1.3.5"
