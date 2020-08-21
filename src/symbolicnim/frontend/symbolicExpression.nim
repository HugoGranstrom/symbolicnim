import macros, sequtils
import ../backend

type SymExpr* = object
  ast*: SymNode

converter intToSymExpr*(d: int): SymExpr = SymExpr(ast: newSymNumber(d.toRational))
converter rationalToSymEXpr*(r: Rational[int]): SymExpr = SymExpr(ast: newSymNumber(r))

proc newSymbol*(name: string): SymExpr =
  SymExpr(ast: newSymbolNode(name))

proc `$`*(symExpr: SymExpr): string =
  $symExpr.ast

proc equal*(a, b: SymExpr): bool =
  a.ast == b.ast

proc copy*(a: SymExpr): SymExpr =
  SymExpr(ast: copySymTree(a.ast))

iterator items*(symExpr: SymExpr): SymExpr =
  let itemSeq = toSeq items(symExpr.ast)
  for sym in itemSeq:
    yield SymExpr(ast: sym)

iterator pairs*(symExpr: SymExpr): (int, SymExpr) =
  let itemsSeq = toSeq items(symExpr.ast)
  for i, sym in itemsSeq:
    yield (i, SymExpr(ast: sym))


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

proc sin*(a: SymExpr): SymExpr =
  SymExpr(ast: sin(a.ast))

proc cos*(a: SymExpr): SymExpr =
  SymExpr(ast: cos(a.ast))

proc tan*(a: SymExpr): SymExpr =
  SymExpr(ast: tan(a.ast))

proc compileSymExpr*(symExpr: SymExpr): NimNode =
  compileSymNode(symExpr.ast)

macro compile*(symExpr: static SymExpr): untyped =
  compileSymExpr(symExpr)

#template generate*(symExpr: SymExpr, signatures: untyped): untyped =
#  generate(symExpr.ast, signatures)

macro createSymbols*(varNames: varargs[untyped]): untyped =
    ## Transforms createVars(x, y) to:
    ## let x = newVariable("x")
    ## let y = newVariable("y")
    result = newStmtList()
    for varName in varNames:
        if varName.kind == nnkIdent:
            let nameStr = varName.strVal
            result.add quote do:
                let `varName` = newSymbol(`nameStr`)
        else:
            raise newException(ValueError, "All arguments must be valid identifiers")

template sym_pi*(): SymExpr =
  newSymbol("π")
