# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest

import symbolicnim

suite "Basic arithmetics tests":
  setup:
    let x = newVariable("x")
    let y = newVariable("y")
    let z = newVariable("z")
    let a = newVariable("a")
    let b = newVariable("b")
  
  test "Negate variable":
    let result1 = -x
    check $result1 == "-1*x"

  test "Add different variables":
    let result1 = x + y
    check $result1 == "x + y"
    let result2 = x + y + z + a + b
    check $result2 == "x + y + z + a + b"
  
  test "Add same variables":
    let result1 = x + x
    check $result1 == "2*x"
    let result2 = x + x + x
    check $result2 == "3*x"
    let result3 = x*2 + 5*x + x
    check $result3 == "8*x"
    let result4 = 2*x + 3*y + 2*x + 7*y
    check $result4 == "4*x + 10*y"
  
  test "Subtract same variable":
    let result1 = 3*x - x
    check $result1 == "2*x"
    let result2 = 5*x - 4*x
    check $result2 == "x"
    let result3 = x - x
    check $result3 == "0"

  test "Multiplication different variables":
    let result1 = a * b
    check $result1 == "a*b"
    let result2 = a * b * z*y
    check $result2 == "a*b*z*y"

  test "Multiplication same variables":
    let result1 = a * a
    check $result1 == "a^2"
    let result2 = a * a * b * a
    check $result2 == "b*a^3"

  test "Division different variables":
    let result1 = x / y
    check $result1 == "x*y^-1"
    let result2 = x * y / (a*b)
    check $result2 == "x*y*a^-1*b^-1"

  test "Division same variables":
    let result1 = x / x
    check $result1 == "1"
    let result2 = x ^ 5 / x ^ 7
    check $result2 == "x^-2"
    let result3 = x ^ 10 / x ^ 4
    check $result3 == "x^6"

  test "Exponent Multiplication":
    let result1 = x ^ 6 * x ^ 4
    check $result1 == "x^10"
    let result2 = x ^ -6 * x ^ 4
    check $result2 == "x^-2"
    let result3 = (a + b + 1) ^ 2 * (b + 1 + a) ^ 3
    check $result3 == "(1 + a + b)^5"
  
  test "Exponent to exponent":
    let result1 = (x ^ 3) ^ y
    let result2 = x ^ 3 ^ y # x^(3^y)
    check not equal(result1, result2)
    check $result1 == "x^(3*y)"

  test "Exponent mult exponent":
    let result1 = (x+1) ^ 2 * (1+x) ^ (y+3)
    check $result1 == "(1 + x)^(5 + y)"

  test "Addition is order-independent":
    check equal(1 + x + 2*y - z*b, -z*b + x + 2*y + 1)
  
  test "Multiplication is order-independent":
    check equal(1 * x * (2+y) / (z+b), 1 / (z+b) * x * (2+y) * 1)

  test "Constant mult terms broadcast":
    let terms = 2*x + 3//2 * y^2 - 6 - z
    let result1 = 2 * terms
    check $result1 == "-12 + 4*x + 3*y^2 + -2*z"

suite "Derivatives & FuncCall":
  setup:
    let x = newVariable("x")
    let y = newVariable("y")
    let z = newVariable("z")
  
  test "diff of constant":
    let result1 = diff(4, x)
    check $result1 == "0"

  test "diff same variable":
    let result1 = diff(x, x)
    check $result1 == "1"
  
  test "diff x^2":
    let xSquare = x ^ 2
    let diff1 = diff(xSquare, x)
    let diff2 = diff(xSquare, x, 2)
    let diff3 = diff(xSquare, x, 3)
    check $diff1 == "2*x"
    check $diff2 == "2"
    check $diff3 == "0"

  test "diff x^x":
    let xToX = x^x
    let diff1 = diff(xToX, x)
    check $diff1 == "x^x + ln(x)*x^x"
    check equal(diff1, x^x + ln(x)*x^x)

  test "sin":
    check $sin(x*y) == "sin(x*y)"
    check $sin(sym_pi) == "0"
    check $sin(0 // 1) == "0"
    check $diff(sin(x), x) == "cos(x)"
    check $diff(sin(2*x), x) == "2*cos(2*x)"

  test "cos":
    check $cos(x*y) == "cos(x*y)"
    check $cos(sym_pi) == "-1"
    check $cos(0 // 1) == "1"
    check $diff(cos(x), x) == "-1*sin(x)"
    check $diff(cos(2*x), x) == "-2*sin(2*x)"

  test "tan":
    check $tan(x*y) == "tan(x*y)"
    check $tan(sym_pi) == "0"
    check $tan(0 // 1) == "0"
    check $diff(tan(x), x) == "cos(x)^-2"
    check $diff(tan(2*x), x) == "2*cos(2*x)^-2"

  test "exp":
    check $exp(x*y) == "exp(x*y)"
    check $exp(0 // 1) == "1"
    check $diff(exp(x), x) == "exp(x)"
    check $diff(exp(2*x), x) == "2*exp(2*x)"

  test "ln":
    expect(ValueError):
      discard ln(0 // 1)
    check $ln(x*y) == "ln(x*y)"
    check $ln(1 // 1) == "0"
    check $diff(ln(x), x) == "x^-1"
    check equal(diff(ln(x), x), 1 / x)
    check $diff(ln(x^2), x) == "2*x^-1"
  
  test "exp & ln":
    let expression = x + y^2 - 3*z
    check equal(ln(exp(expression)), exp(ln(expression)))
    check $ln(exp(expression)) == $expression



