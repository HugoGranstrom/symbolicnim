import unittest

import symbolicnim
import symbolicnim/backend


suite "Basic arithmetics tests":
  setup:
    let x = newSymbol("x")
    let y = newSymbol("y")
    let z = newSymbol("z")
    let a = newSymbol("a")
    let b = newSymbol("b")
  
  test "Hej":
    #echo (x + y).ast.subs(x.ast, newSymNumber(1//1))
    echo subs(sin(x + y), x + y, z)
    echo (x + x*y*z).subs(x*y, z)

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
    check $result1 == "-x"

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

  test "Constant mult terms broadcast":
    let terms = 2*x + 3//2 * y^2 - 6 - z
    let result1 = 2 * terms
    check $result1 == "-12 + 4*x - 2*z + 3*y^2"

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
    check $diff(cos(x), x) == "-sin(x)"
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

  test "A bunch of expressions to check if strings are right":
    createSymbols(x0)
    let result1 = 1 + (x-x0) + (x-x0)^2 + (x-x0)^3 + (x-x0)^4 + (x-x0)^5
    check $result1 == "1 + x - x0 + (x - x0)^2 + (x - x0)^3 + (x - x0)^4 + (x - x0)^5"
    let result2 = x - 1 + y - a + b - 3
    check $result2 == "-4 - a + b + x + y"
    let result3 = (a*b)*(z + y) * x^y / a / z
    check $result3 == "b*x^y*z^-1*(y + z)"

  test "copySymNode & copySymTree":
    let expr1 = sin(x)
    # we must set the hashes so it doesn't get a strange default value
    expr1.ast.hashcache = 10
    expr1.ast.children[0].hashcache = 10
    check expr1.ast.hashcache == 10
    check expr1.ast.children[0].hashcache == 10
    let nodeCopy = SymExpr(ast: copySymNode(expr1.ast))
    check nodeCopy.ast.hashcache == 10
    check nodeCopy.ast.children[0].hashcache == 10
    let treeCopy = SymExpr(ast: copySymTree(expr1.ast))
    check treeCopy.ast.hashcache == 10
    check treeCopy.ast.children[0].hashcache == 10
    check expr1.ast.hashcache == 10
    check expr1.ast.children[0].hashcache == 10
    treeCopy.ast.hashCache = 1
    treeCopy.ast.children[0].hashCache = 2
    # The others shouldn't be affected:
    check expr1.ast.hashcache == 10
    check expr1.ast.children[0].hashcache == 10
    check nodeCopy.ast.hashcache == 10
    check nodeCopy.ast.children[0].hashcache == 10
    nodeCopy.ast.hashCache = 3
    nodeCopy.ast.children[0].hashCache = 4
    check expr1.ast.hashcache == 10 # this is unaffected because we copied it
    check expr1.ast.children[0].hashcache == 4 # this is affected because it is deeper than we copied
    check treeCopy.ast.hashcache == 1
    check treeCopy.ast.children[0].hashcache == 2

  test "Iterate over SymNode":
    let symNodes = @[x, 5, x^4, sin(x*y), a + b - z * 6 + x^(2-a), x*y*3/a * (b - 3) / (x + x + y)]
    let correctStrings = @[@["x"], @["5"], @["x^4"], @["x*y"], @["a", "b", "-6*z", "x^(2 - a)"], @["3", "a^-1", "x", "y", "-3 + b", "(2*x + y)^-1"]]
    for i, node in symNodes:
      for j, child in node:
        check $child == correctStrings[i][j]

  test "is a ?":
    check isNumber(x - x + 1) == true
    check isNumber(2*x - x + 1) == false
    check isSymbol(x) == true
    check isSymbol(-x) == false
    check isFunc(sin(x+y)) == true
    check isFunc(-sin(x+y)) == false
    check isPow(x^2) == true
    check isPow(2*x^3) == false
    check isAdd(2*x + y) == true
    check isAdd(x*(x+y)) == false
    check isMul(x*(x+y)) == true
    check isMul(-x + 1) == false

  echo "Subs"
  test "subs symbol":
    check $subs(x, x, z) == "z"
    check $subs(2 + y + x, x, z) == "2 + y + z"
    check $subs(sin(x), x, z) == "sin(z)"
    check $subs(2 * x * y, x , z) == "2*y*z"
    check $subs(x^x, x, z) == "z^z"

  test "subs func":
    check $subs(sin(x), x, 0) == "0"
    check $subs(sin(x), sin(x), a) == "a"
    check $subs(2+x+sin(2*y), 2*y, b) == "2 + x + sin(b)"
    check $subs(2+x+sin(2*y), sin(2*y), b) == "2 + b + x"
    check $subs(2*x*sin(2*y), sin(2*y), b) == "2*b*x"
    check $subs(sin(sin(x)), sin(x), z) == "sin(z)"
    check $subs(sin(x)^sin(x), sin(x), a) == "a^a"

  echo "Macros"
  test "createVars":
    createSymbols(k, j, cool)
    check (typeof(k) is SymExpr)
    check (k.ast.name == "k")
    check (typeof(j) is SymExpr)
    check (j.ast.name == "j")
    check (typeof(cool) is SymEXpr)
    check (cool.ast.name == "cool")


