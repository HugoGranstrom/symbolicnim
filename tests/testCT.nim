import unittest, rationals
import symbolicnim/[backend, frontend]

converter intToSymNumber(d: int): SymNode = newSymNumber(d.toRational)
converter rationalToSymNumber(d: Rational[int]): SymNode = newSymNumber(d)

suite "Basic arithmetics tests":
  setup:
    let x = newSymbolNode("x")
    let y = newSymbolNode("y")
    let z = newSymbolNode("z")
    let a = newSymbolNode("a")
    let b = newSymbolNode("b")

  test "0*x = 0":
    let result1 = 0 * x
    check $result1 == "0"
    let result2 = x * 2 * 4 * 0 * y * 3
    check $result2 == "0"

  test "exp":
    let result1 = exp(2*x)
    echo result1
    echo exp(x - x)
    echo diff_internal(result1 + x, x)

  test "ln":
    let result1 = ln(2*x)
    echo result1
    echo ln(1 + x - x)
    echo diff_internal(result1, x)

  test "diff power":
    echo "d/dx(x^2) = ", diff_internal(x^2, x)
    echo "d/dx(y^x) = ", diff_internal(y^x, x)
    echo "d/dx(x^x) = ", diff_internal(x^x, x)

  test "SymExpr":
    let k = newSymbol("k")
    let p = newSymbol("p")
    echo k + p
    echo diff(exp(2*k + p), k, p, k, p)

echo "Compiletime:"
let x {.compileTime.} = newSymbolNode("x")
let y {.compileTime.} = newSymbolNode("y")
(exp(x) + ln(y)).generate:
  proc f(x, y: float): float

echo "f(1.0, 2.0) = ", f(1.0, 2.0)

let k {.compileTime.} = newSymbol("k")
let p {.compileTime.} = newSymbol("p")
(k + p).generate:
  proc kj(k, p: float): float
echo "kj(2.0, 3.0) = ", kj(2.0, 3.0)

