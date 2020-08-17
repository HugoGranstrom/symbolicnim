import unittest, rationals
import symbolicnim/backend

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

echo "Compiletime:"
let x {.compileTime.} = newSymbolNode("x")
let y {.compileTime.} = newSymbolNode("y")
(exp(x) + ln(y)).generate:
  proc f(x, y: float): float

echo "f(1.0, 2.0) = ", f(1.0, 2.0)

