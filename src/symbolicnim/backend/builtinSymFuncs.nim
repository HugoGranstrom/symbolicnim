import rationals, macros

### Move all this into extensions? Must solve circular dependecies! Move diff_internal into utils? 
proc exp*(symNode: SymNode): SymNode =
  if symNode.kind == symFunc and symNode.funcName == "ln":
    return symNode.children[0]
  if symNode.kind == symNumber and symNode.lit == 0 // 1:
    return newSymNumber(1 // 1)
  result = newSymNode(symFunc)
  result.funcName = "exp"
  result.nargs = 1
  result.children.add symNode

proc diffExp*(symNode: SymNode, dVar: SymNode): SymNode =
  assert symNode.kind == symFunc and symNode.funcName == "exp"
  # calculate d/dx(exp(f(x))) = d/dx(f(x)) * exp(f(x))
  let child = symNode.children[0]
  result = diff_internal(child, dVar) * symNode

proc compileExp*(symNode: SymNode): NimNode =
  assert symNode.kind == symFunc and symNode.funcName == "exp"
  # generate the code `exp(compile(symNode.children[0]))`
  let childNimNode = compileSymNode(symNode.children[0])
  result = quote do:
    exp(`childNimNode`)

registerSymFunc("exp", diffExp, compileExp)