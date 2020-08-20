import unittest

import symbolicnim

template testBlock(name: string, code: untyped): untyped =
  echo "Running compiletime test " & name
  block:
    code

static:
  let x = newSymbol("x")
  let y = newSymbol("y")
  let z = newSymbol("z")
  let a = newSymbol("a")
  let b = newSymbol("b")
  testBlock "0*x = 0":
    let result1 = 0 * x
    doAssert $result1 == "0"
    let result2 = x * 2 * 4 * 0 * y * 3
    doAssert $result2 == "0"

  testBlock "1*x = x":
    let result1 = x * 1
    doAssert $result1 == "x"
    let result2 = 2*x*3*1
    doAssert $result2 == "6*x"

  testBlock "Negate variable":
    let result1 = -x
    doAssert $result1 == "-1*x"

  testBlock "Add different variables":
    let result1 = x + y
    doAssert $result1 == "x + y"
    let result2 = x + y + z + a + b
    doAssert $result2 == "a + b + x + y + z"
  
  testBlock "Add same variables":
    let result1 = x + x
    doAssert $result1 == "2*x"
    let result2 = x + x + x
    doAssert $result2 == "3*x"
    let result3 = x*2 + 5*x + x
    doAssert $result3 == "8*x"
    let result4 = 2*x + 3*y + 2*x + 7*y
    doAssert $result4 == "4*x + 10*y"
  
  testBlock "Subtract same variable":
    let result1 = 3*x - x
    doAssert $result1 == "2*x"
    let result2 = 5*x - 4*x
    doAssert $result2 == "x"
    let result3 = x - x
    doAssert $result3 == "0"

  testBlock "Multiplication different variables":
    let result1 = a * b
    doAssert $result1 == "a*b"
    let result2 = a * b * z*y
    doAssert $result2 == "a*b*y*z"

  testBlock "Multiplication same variables":
    let result1 = a * a
    doAssert $result1 == "a^2"
    let result2 = a * a * b * a
    doAssert $result2 == "a^3*b"

  testBlock "Division different variables":
    let result1 = x / y
    doAssert $result1 == "x*y^-1"
    let result2 = x * y / (a*b)
    doAssert $result2 == "a^-1*b^-1*x*y"

  testBlock "Division same variables":
    let result1 = x / x
    doAssert $result1 == "1"
    let result2 = x ^ 5 / x ^ 7
    doAssert $result2 == "x^-2"
    let result3 = x ^ 10 / x ^ 4
    doAssert $result3 == "x^6"

  testBlock "Exponent Multiplication":
    let result1 = x ^ 6 * x ^ 4
    doAssert $result1 == "x^10"
    let result2 = x ^ -6 * x ^ 4
    doAssert $result2 == "x^-2"
    let result3 = (a + b + 1) ^ 2 * (b + 1 + a) ^ 3
    doAssert $result3 == "(1 + a + b)^5"
  
  testBlock "Exponent to exponent":
    let result1 = (x ^ 3) ^ y
    let result2 = x ^ 3 ^ y # x^(3^y)
    doAssert not equal(result1, result2)
    doAssert $result1 == "x^(3*y)"

  testBlock "Exponent mult exponent":
    let result1 = (x+1) ^ 2 * (1+x) ^ (y+3)
    doAssert $result1 == "(1 + x)^(5 + y)"

  testBlock "Addition is order-independent":
    doAssert equal(1 + x + 2*y - z*b, -z*b + x + 2*y + 1)
  
  testBlock "Multiplication is order-independent":
    doAssert equal(1 * x * (2+y) / (z+b), 1 / (z+b) * x * (2+y) * 1)

  #[testBlock "Constant mult terms broadcast":
    let terms = 2*x + 3//2 * y^2 - 6 - z
    let result1 = 2 * terms
    doAssert $result1 == "-12 + 4*x + 3*y^2 + -2*z"]#

  testBlock "not equal":
    doAssert not equal(x, y)
    doAssert not equal(x + 1, x - 1)
    doAssert not equal(y*x + x*y, x*y)

  #[testBlock "+=":
    var exp1 = x^2
    exp1 += 2*x
    doAssert $exp1 == "x^2 + 2*x"
    exp1 += 1
    doAssert $exp1 == "1 + x^2 + 2*x"]#

  echo "Derivatives and FuncCall"
  
  testBlock "diff of constant":
    let result1 = diff(4, x)
    doAssert $result1 == "0"

  testBlock "diff same variable":
    let result1 = diff(x, x)
    doAssert $result1 == "1"

  testBlock "diff different variable":
    let result1 = diff(x, y)
    doAssert $result1 == "0"
  
  testBlock "diff x^2":
    let xSquare = x ^ 2
    let diff1 = diff(xSquare, x)
    let diff2 = diff(xSquare, x, 2)
    let diff3 = diff(xSquare, x, 3)
    doAssert $diff1 == "2*x"
    doAssert $diff2 == "2"
    doAssert $diff3 == "0"

  testBlock "diff x^x":
    let xToX = x^x
    let diff1 = diff(xToX, x)
    doAssert $diff1 == "x^x + x^x*ln(x)"
    doAssert equal(diff1, x^x + ln(x)*x^x)

  testBlock "sin":
    doAssert $sin(x*y) == "sin(x*y)"
    doAssert $sin(sym_pi) == "0"
    doAssert $sin(0 // 1) == "0"
    doAssert $diff(sin(x), x) == "cos(x)"
    doAssert $diff(sin(2*x), x) == "2*cos(2*x)"

  testBlock "cos":
    doAssert $cos(x*y) == "cos(x*y)"
    doAssert $cos(sym_pi) == "-1"
    doAssert $cos(0 // 1) == "1"
    doAssert $diff(cos(x), x) == "-1*sin(x)"
    doAssert $diff(cos(2*x), x) == "-2*sin(2*x)"

  testBlock "tan":
    doAssert $tan(x*y) == "tan(x*y)"
    doAssert $tan(sym_pi) == "0"
    doAssert $tan(0 // 1) == "0"
    doAssert $diff(tan(x), x) == "cos(x)^-2"
    doAssert $diff(tan(2*x), x) == "2*cos(2*x)^-2"

  testBlock "exp":
    doAssert $exp(x*y) == "exp(x*y)"
    doAssert $exp(0 // 1) == "1"
    doAssert $diff(exp(x), x) == "exp(x)"
    doAssert $diff(exp(2*x), x) == "2*exp(2*x)"

  testBlock "ln":
    #expect(ValueError):
    #  discard ln(0 // 1)
    doAssert $ln(x*y) == "ln(x*y)"
    doAssert $ln(1 // 1) == "0"
    doAssert $diff(ln(x), x) == "x^-1"
    doAssert equal(diff(ln(x), x), 1 / x)
    doAssert $diff(ln(x^2), x) == "2*x^-1"
  
  testBlock "exp & ln":
    let expression = x + y^2 - 3*z
    doAssert equal(ln(exp(expression)), exp(ln(expression)))
    doAssert $ln(exp(expression)) == $expression

  echo "Macros"
  testBlock "createVars":
    createSymbols(k, j, cool)
    doAssert (typeof(k) is SymExpr)
    doAssert (k.ast.name == "k")
    doAssert (typeof(j) is SymExpr)
    doAssert (j.ast.name == "j")
    doAssert (typeof(cool) is SymEXpr)
    doAssert (cool.ast.name == "cool")

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

  test "1*x = x":
    let result1 = x * 1
    check $result1 == "x"
    let result2 = 2*x*3*1
    check $result2 == "6*x"

  test "Negate variable":
    let result1 = -x
    check $result1 == "-1*x"

  test "Add different variables":
    let result1 = x + y
    check $result1 == "x + y"
    let result2 = x + y + z + a + b
    check $result2 == "a + b + x + y + z"
  
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
    check $result2 == "a*b*y*z"

  test "Multiplication same variables":
    let result1 = a * a
    check $result1 == "a^2"
    let result2 = a * a * b * a
    check $result2 == "a^3*b"

  test "Division different variables":
    let result1 = x / y
    check $result1 == "x*y^-1"
    let result2 = x * y / (a*b)
    check $result2 == "a^-1*b^-1*x*y"

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

  #[test "Constant mult terms broadcast":
    let terms = 2*x + 3//2 * y^2 - 6 - z
    let result1 = 2 * terms
    check $result1 == "-12 + 4*x + 3*y^2 + -2*z"]#

  test "not equal":
    check not equal(x, y)
    check not equal(x + 1, x - 1)
    check not equal(y*x + x*y, x*y)

  #[test "+=":
    var exp1 = x^2
    exp1 += 2*x
    check $exp1 == "x^2 + 2*x"
    exp1 += 1
    check $exp1 == "1 + x^2 + 2*x"]#

  echo "Derivatives and FuncCall"
  
  test "diff of constant":
    let result1 = diff(4, x)
    check $result1 == "0"

  test "diff same variable":
    let result1 = diff(x, x)
    check $result1 == "1"

  test "diff different variable":
    let result1 = diff(x, y)
    check $result1 == "0"
  
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
    check $diff1 == "x^x + x^x*ln(x)"
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
    #expect(ValueError):
    #  discard ln(0 // 1)
    check $ln(x*y) == "ln(x*y)"
    check $ln(1 // 1) == "0"
    check $diff(ln(x), x) == "x^-1"
    check equal(diff(ln(x), x), 1 / x)
    check $diff(ln(x^2), x) == "2*x^-1"
  
  test "exp & ln":
    let expression = x + y^2 - 3*z
    check equal(ln(exp(expression)), exp(ln(expression)))
    check $ln(exp(expression)) == $expression

  echo "Macros"
  test "createVars":
    createSymbols(k, j, cool)
    check (typeof(k) is SymExpr)
    check (k.ast.name == "k")
    check (typeof(j) is SymExpr)
    check (j.ast.name == "j")
    check (typeof(cool) is SymEXpr)
    check (cool.ast.name == "cool")


