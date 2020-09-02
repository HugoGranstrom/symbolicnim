import symbolicnim
import symbolicnim/backend

template testBlock(name: string, code: untyped): untyped =
  echo "Running test " & name
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
    doAssert $result1 == "-x"

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

  testBlock "Constant mult terms broadcast":
    let terms = 2*x + 3//2 * y^2 - 6 - z
    let result1 = 2 * terms
    doAssert $result1 == "-12 + 4*x - 2*z + 3*y^2"

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
    doAssert $diff(cos(x), x) == "-sin(x)"
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

  testBlock "A bunch of expressions to check if strings are right":
    createSymbols(x0)
    let result1 = 1 + (x-x0) + (x-x0)^2 + (x-x0)^3 + (x-x0)^4 + (x-x0)^5
    doAssert $result1 == "1 + x - x0 + (x - x0)^2 + (x - x0)^3 + (x - x0)^4 + (x - x0)^5"
    let result2 = x - 1 + y - a + b - 3
    doAssert $result2 == "-4 - a + b + x + y"
    let result3 = (a*b)*(z + y) * x^y / a / z
    doAssert $result3 == "b*x^y*z^-1*(y + z)"

  testBlock "copySymNode & copySymTree":
    let expr1 = sin(x)
    # we must set the hashes so it doesn't get a strange default value
    expr1.ast.hashcache = 10
    expr1.ast.children[0].hashcache = 10
    doAssert expr1.ast.hashcache == 10
    doAssert expr1.ast.children[0].hashcache == 10
    let nodeCopy = SymExpr(ast: copySymNode(expr1.ast))
    doAssert nodeCopy.ast.hashcache == 10
    doAssert nodeCopy.ast.children[0].hashcache == 10
    let treeCopy = SymExpr(ast: copySymTree(expr1.ast))
    doAssert treeCopy.ast.hashcache == 10
    doAssert treeCopy.ast.children[0].hashcache == 10
    doAssert expr1.ast.hashcache == 10
    doAssert expr1.ast.children[0].hashcache == 10
    treeCopy.ast.hashCache = 1
    treeCopy.ast.children[0].hashCache = 2
    # The others shouldn't be affected:
    doAssert expr1.ast.hashcache == 10
    doAssert expr1.ast.children[0].hashcache == 10
    doAssert nodeCopy.ast.hashcache == 10
    doAssert nodeCopy.ast.children[0].hashcache == 10
    nodeCopy.ast.hashCache = 3
    nodeCopy.ast.children[0].hashCache = 4
    doAssert expr1.ast.hashcache == 10 # this is unaffected because we copied it
    doAssert expr1.ast.children[0].hashcache == 4 # this is affected because it is deeper than we copied
    doAssert treeCopy.ast.hashcache == 1
    doAssert treeCopy.ast.children[0].hashcache == 2
  
  testBlock "Iterate over SymNode":
    let symNodes = @[x, 5, x^4, sin(x*y), a + b - z * 6 + x^(2-a), x*y*3/a * (b - 3) / (x + x + y)]
    let correctStrings = @[@["x"], @["5"], @["x^4"], @["x*y"], @["a", "b", "-6*z", "x^(2 - a)"], @["3", "a^-1", "x", "y", "-3 + b", "(2*x + y)^-1"]]
    for i, node in symNodes:
      for j, child in node:
        doAssert $child == correctStrings[i][j]

  testBlock "is a ?":
    doAssert isNumber(x - x + 1) == true
    doAssert isNumber(2*x - x + 1) == false
    doAssert isSymbol(x) == true
    doAssert isSymbol(-x) == false
    doAssert isFunc(sin(x+y)) == true
    doAssert isFunc(-sin(x+y)) == false
    doAssert isPow(x^2) == true
    doAssert isPow(2*x^3) == false
    doAssert isAdd(2*x + y) == true
    doAssert isAdd(x*(x+y)) == false
    doAssert isMul(x*(x+y)) == true
    doAssert isMul(-x + 1) == false
  
  echo "Subs"
  testBlock "subs symbol":
    doAssert $subs(x, x, z) == "z"
    doAssert $subs(2 + y + x, x, z) == "2 + y + z"
    doAssert $subs(sin(x), x, z) == "sin(z)"
    doAssert $subs(2 * x * y, x , z) == "2*y*z"
    doAssert $subs(x^x, x, z) == "z^z"

  testBlock "subs func":
    doAssert $subs(sin(x), x, 0) == "0"
    doAssert $subs(sin(x), sin(x), a) == "a"
    doAssert $subs(2+x+sin(2*y), 2*y, b) == "2 + x + sin(b)"
    doAssert $subs(2+x+sin(2*y), sin(2*y), b) == "2 + b + x"
    doAssert $subs(2*x*sin(2*y), sin(2*y), b) == "2*b*x"
    doAssert $subs(sin(sin(x)), sin(x), z) == "sin(z)"
    doAssert $subs(sin(x)^sin(x), sin(x), a) == "a^a"

  testBlock "subs pow":
    doAssert $subs(x^2, x^2, z) == "z"
    doAssert $subs(x, x^2, z) == "x"
    doAssert $subs(1 + x + x^y, x^y, z) == "1 + x + z"
    doAssert $subs(3*y*x^y, x^y, z) == "3*y*z"
    doAssert $subs(sin(y^x), y^x, z) == "sin(z)"

  testBlock "subs mul":
    doAssert $subs(x, 2*x, z) == "x"
    doAssert $subs(2*x, 2*x, z) == "z"
    doAssert $subs(2*x*y, 2*x, z) == "y*z"
    doAssert $subs(2*x^z*y, 2*x, z) == "2*x^z*y"
    doAssert $subs(2*x*y*z, y*z, a) == "2*a*x"
    doAssert $subs(3 + x*y + z, x*y, a) == "3 + a + z"
    doAssert $subs(3 + 2*x*y + z, x*y, a) == "3 + 2*a + z"
    doAssert $subs((x*y)^(x*y), x*y, z) == "z^z"

  testBlock "subs add":
    doAssert $subs(x, 2 + x, z) == "x"
    doAssert $subs(2 + x, 2 + x, z) == "z"
    doAssert $subs(2 + x + y, 2 + x, z) == "y + z"
    doAssert $subs(2 + 2*x + y, 2 + x, z) == "2 + 2*x + y"
    doAssert $subs(2 + x + y + z, y + z, a) == "2 + a + x"
    doAssert $subs(3*z*(x + y), x + y, a) == "3*a*z"
    doAssert $subs(3*z*(x + y + z), x + y, a) == "3*z*(a + z)"
    doAssert $subs((x+y)^(x+y), x+y, a) == "a^a"
  
  echo "Expansions"
  testBlock "Taylor expansion":
    let t = taylor(sin(x), x, 0, 5)
    doAssert $t == "x - 1/6*x^3 + 1/120*x^5"
    let t2 = taylor(sin(x), x, a, 3)
    #check $t2 == "sin(a) - 1/6*cos(a)*(-a + x)^3 - 1/2*sin(a)*(-a + x)^2 + cos(a)*(-a + x)"
    doAssert equal(t2, sin(a) - 1//6*cos(a)*(-a + x)^3 - 1//2*sin(a)*(-a + x)^2 + cos(a)*(-a + x))

  echo "Macros"
  testBlock "createVars":
    createSymbols(k, j, cool)
    doAssert (typeof(k) is SymExpr)
    doAssert (k.ast.name == "k")
    doAssert (typeof(j) is SymExpr)
    doAssert (j.ast.name == "j")
    doAssert (typeof(cool) is SymEXpr)
    doAssert (cool.ast.name == "cool")

import math

let x {.compileTime.} = newSymbol("x")
let y {.compileTime.} = newSymbol("y")
let z {.compileTime.} = newSymbol("z")
let exprToCompile {.compileTime.} = exp(x*y) ^ sin(z/x) + 1
exprToCompile.generate:
  proc f(x, y, z: float): float
  proc fInline(x, y, z: float): float {.inline.}

testBlock "Eval generated proc":
  let xVal = 1.2345
  let yVal = 6.5432
  let zVal = 10.918273645
  doAssert f(xVal, yVal, zVal) == pow(exp(xVal*yVal), sin(zVal / xVal)) + 1.0 
  doAssert fInline(xVal, yVal, zVal) == pow(exp(xVal*yVal), sin(zVal / xVal)) + 1.0  