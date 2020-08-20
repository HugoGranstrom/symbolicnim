# Package

version       = "0.2.2"
author        = "HugoGranstrom"
description   = "A symbolic algebra library written in Nim"
license       = "MIT"
srcDir        = "src"

task test, "Runs the test suite":
  const filenames = ["tests/testSuite.nim", "tests/testSuiteCT.nim"]
  when defined(windows):
    const binaryExt = ".exe"
  else:
    const binaryExt = ""
  echo "####################\nRunning tests using Refc GC:\n####################"
  for name in filenames:
    exec "nim c -r --gc:refc -o:bin/testSuite" & binaryExt &  " " & name
  echo "####################\nRunning tests using GC:ARC:\n####################"
  for name in filenames:
    exec "nim c -r --gc:arc -o:bin/testSuite" & binaryExt &  " " & name
  echo "####################\nRunning tests using GC:ORC:\n####################"
  for name in filenames:
    exec "nim c -r --gc:orc -o:bin/testSuite" & binaryExt &  " " & name

# Dependencies

requires "nim >= 1.2.6"
