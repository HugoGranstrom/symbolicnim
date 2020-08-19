import symbolicnim
import symbolicnim/oldVersion
import numericalnim

proc oldBench(N: int): bool =
  createVars(x, y, z)
  var total: SymbolicExpression = 0
  for i in 0 .. N:
    if i mod 2 == 0:
      if i mod 3 == 0:
        total = total * x
      elif i mod 3 == 1:
        total = total + y
      elif i mod 3 == 2:
        total = total ^ z
    else:
      if i mod 3 == 0:
        total = total * 2
      elif i mod 3 == 1:
        total = total + 3
      elif i mod 3 == 2:
        total = total ^ (3 // 2)
    total = exp(total)

proc newBench(N: int): bool =
  createSymbols(x, y, z)
  var total: SymExpr = 0
  for i in 0 .. N:
    if i mod 2 == 0:
      if i mod 3 == 0:
        total = total * x
      elif i mod 3 == 1:
        total = total + y
      elif i mod 3 == 2:
        total = total ^ z
    else:
      if i mod 3 == 0:
        total = total * 2
      elif i mod 3 == 1:
        total = total + 3
      elif i mod 3 == 2:
        total = total ^ (3 // 2)
    total = exp(total)

let M = 100
let N = 100

timeit(oldBench(M), N, "Old")
timeit(newBench(M), N, "New")