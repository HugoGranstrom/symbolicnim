import unittest, rationals
import symbolicnim/[backend, frontend]

converter intToSymNumber(d: int): SymNode = newSymNumber(d.toRational)
converter rationalToSymNumber(d: Rational[int]): SymNode = newSymNumber(d)

suite "Basic arithmetics tests":
  setup:
    let x = newSymbol("x")
    let y = newSymbol("y")
    let z = newSymbol("z")
    let a = newSymbol("a")
    let b = newSymbol("b")

  test "0*x = 0":
    let result1 = 0 * x
    check $result1 == "0"
    let result2 = x * 2 * 4 * 0 * y * 3
    check $result2 == "0"

  test "exp":
    let result1 = exp(2*x)
    echo result1
    echo exp(x - x)
    echo diff(result1 + x, x)

  test "ln":
    let result1 = ln(2*x)
    echo result1
    echo ln(1 + x - x)
    echo diff(result1, x)

  test "diff power":
    echo "d/dx(x^2) = ", diff(x^2, x)
    echo "d/dx(y^x) = ", diff(y^x, x)
    echo "d/dx(x^x) = ", diff(x^x, x)

  test "SymExpr":
    let k = newSymbol("k")
    let p = newSymbol("p")
    echo k + p
    echo diff(exp(2*k + p), k, p, k, p)

  test "SymMatrix":
    let A = @[x, y, z].toRow
    let B = @[x, y*x*y + z*2-5*x + x^4*z, z].toCol
    echo A * B

echo "Compiletime:"
let x {.compileTime.} = newSymbol("x")
let y {.compileTime.} = newSymbol("y")
let z {.compileTime.} = newSymbol("z")
(exp(x) + ln(y)).generate:
  proc f(x, y: float): float

echo "f(1.0, 2.0) = ", f(1.0, 2.0)

let k {.compileTime.} = newSymbol("k")
let p {.compileTime.} = newSymbol("p")
(k + p).generate:
  proc kj(k, p: float): float
echo "kj(2.0, 3.0) = ", kj(2.0, 3.0)

let A {.compileTime.} = @[x, y, z].toRow
let B {.compileTime.} = @[x, y, z].toCol
(A*B).generate:
      proc matrixMult(x, y, z: float): Tensor[float]
echo matrixMult(1.0, 2.0, 3.0)

