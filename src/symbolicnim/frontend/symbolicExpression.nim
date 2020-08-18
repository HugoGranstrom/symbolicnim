import ../backend

type SymExpr* = ref object
  ast*: SymNode

converter intToSymExpr*(d: int): SymExpr = SymExpr(ast: newSymNumber(d.toRational))
converter rationalToSymEXpr*(r: Rational[int]): SymExpr = SymExpr(ast: newSymNumber(r))

proc newSymbol*(name: string): SymExpr =
  SymExpr(ast: newSymbolNode(name))

proc `$`*(symExpr: SymExpr): string =
  $symExpr.ast

proc equal*(a, b: SymExpr): bool =
  a.ast == b.ast

proc `+`*(a, b: SymExpr): SymExpr =
  SymExpr(ast: a.ast + b.ast)

proc `-`*(a, b: SymExpr): SymExpr =
  SymExpr(ast: a.ast - b.ast)

proc `*`*(a, b: SymExpr): SymExpr =
  SymExpr(ast: a.ast * b.ast)

proc `/`*(a, b: SymExpr): SymExpr =
  SymExpr(ast: a.ast / b.ast)

proc `^`*(a, b: SymExpr): SymExpr =
  SymExpr(ast: a.ast ^ b.ast)

proc `-`*(a: SymExpr): SymExpr =
  SymExpr(ast: -a.ast)

proc diff*(symExpr: SymExpr, dVar: SymExpr, derivOrder: Natural = 1): SymExpr =
  SymExpr(ast: diff(symExpr.ast, dVar.ast, derivOrder))

proc diff*(symExpr: SymExpr, dVars: varargs[SymExpr]): SymExpr =
  var dVarsNodes = newSeq[SymNode](dVars.len)
  for i in 0 .. dVars.high:
    dVarsNodes[i] = dVars[i].ast
  result = SymExpr(ast: diff(symExpr.ast, dVarsNodes))

proc exp*(a: SymExpr): SymExpr =
  SymExpr(ast: exp(a.ast))

proc ln*(a: SymExpr): SymExpr =
  SymExpr(ast: ln(a.ast))

proc compileSymExpr*(symExpr: SymExpr): NimNode =
  compileSymNode(symExpr.ast)

macro compile*(symExpr: static SymExpr): untyped =
  compileSymExpr(symExpr)

#template generate*(symExpr: SymExpr, signatures: untyped): untyped =
#  generate(symExpr.ast, signatures)