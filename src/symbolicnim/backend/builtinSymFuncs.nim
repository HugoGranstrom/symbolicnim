import
  ./ast_types,
  ./deepEmbedding,
  ./extensions

proc exp*(symNode: SymNode): SymNode =
  result = newSymNode(symFunc)
  result.funcName = "exp"
  result.nargs = 1
  # do checks for simplification here. for example exp(ln).
  # can we simulate enum by using a proc that checks that funcName is in diffProcs, but only when debug.
  # how to check if running in CT or RT? when nvm?
  result.children.add symNode

proc diffExp*(symNode: SymNode, dVar: SymNode): SymNode =
  assert symNode.kind == symFunc and symNode.funcName == "exp"
  # calculate d/dx(exp(f(x))) = d/dx(f(x)) * exp(f(x))

proc compileExp*(symNode: SymNode): NimNode =
  assert symNode.kind == symFunc and symNode.funcName == "exp"
  # generate the code `exp(compile(symNode.children[0]))`

registerSymFunc("exp", diffExp, compileExp)