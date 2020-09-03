import ./symbolicExpression

type
  TaylorExpansion* = object
    derivs*: seq[SymExpr]
    derivVar*: SymExpr
    evalPoint*: SymExpr
    #currentFactorial*: int

converter taylorToSymExpr*(taylor: TaylorExpansion): SymExpr =
  let x = taylor.derivVar
  let x0 = taylor.evalPoint
  assert (not isNil(x.ast)), "x is nil"
  assert (not isNil(x0.ast)), "x is nil"
  result = taylor.derivs[0].subs(x, x0)
  if taylor.derivs.len == 1: return
  var currentFactorial: int = 1
  let baseFactor = x - x0
  var currentFactor: SymExpr = 1.intToSymExpr
  for i in 1 .. taylor.derivs.high:
    currentFactorial *= i
    currentFactor = currentFactor * baseFactor
    result = result + currentFactor * (taylor.derivs[i].subs(x, x0) / currentFactorial)

proc `$`*(taylor: TaylorExpansion): string = 
  $taylor.taylorToSymExpr

proc taylor*(f: SymExpr, x, x0: SymExpr, n: Natural): TaylorExpansion =
  ## f is the function that is expanded
  ## x is the variable we want to expand w.r.t
  ## x0 is the point we want to expand around
  ## n is the order of the expansion
  assert x.isSymbol, "x must be a symbol"
  assert n <= 20, "n must be 20 or lower to avoid overflow"
  result.derivs.add f
  result.evalPoint = x0
  result.derivVar = x
  if n == 0: return
  for i in 1 .. n:
    result.derivs.add diff(result.derivs[i-1], x)