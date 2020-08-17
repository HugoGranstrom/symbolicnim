import rationals, macros, tables
import
  ./ast_types,
  ./extensions

macro compile*(s: static string): untyped =
  result = quote do:
    echo `s`, " NIm"

proc sumNimNodes*(s: seq[NimNode]): NimNode =
  doAssert s.len > 0, "sumNimNodes must get a seq of length 1 or more."
  if s.len == 1:
    return s[0]
  result = s[0]
  for i in 1 .. s.high:
    let sI = s[i]
    result = quote do:
      `result` + `sI`

proc mulNimNodes*(s: seq[NimNode]): NimNode =
  doAssert s.len > 0, "mulNimNodes must get a seq of length 1 or more."
  if s.len == 1:
    return s[0]
  result = s[0]
  for i in 1 .. s.high:
    let sI = s[i]
    result = quote do:
      `result` * `sI`

proc rationalToNimNode*(r: Rational[int]): NimNode =
  # Rewrites rational to ordinary division between two ints
  let num = newLit r.num
  if r.den == 1: # x/1 = x
    return num
  let den = newLit r.den
  result = quote do:
    `num` / `den`

proc compileSymNode*(symNode: SymNode): NimNode {.compileTime.} =
  case symNode.kind
  of symSymbol:
    result = ident(symNode.name)
  of symNumber:
    result = rationalToNimNode(symNode.lit)
  of symAdd:
    var nimNodes = newSeq[NimNode]()
    if symNode.constant != 0 // 1:
      nimNodes.add rationalToNimNode(symNode.constant)
    for term, coeff in pairs(symNode.terms):
      let termNimNode = compileSymNode(term)
      if coeff != 1 // 1:
        let coeffNimNode = rationalToNimNode(coeff)
        nimNodes.add quote do:
          `coeffNimNode` * `termNimNode`
      else:
        nimNodes.add termNimNode
    result = sumNimNodes(nimNodes)
  of symMul:
    var nimNodes = newSeq[NimNode]() # stores the things that are multiplied
    if symNode.coeff != 1 // 1:
      nimNodes.add rationalToNimNode(symNode.coeff)
    for base, exponent in pairs(symNode.products):
      let baseNimNode = compileSymNode(base)
      if exponent.kind == symNumber and exponent.lit != 1 // 1: 
        let exponentNimNode = compileSymNode(exponent)
        nimNodes.add quote do:
          pow(`baseNimNode`, `exponentNimNOde`)
      else:
        nimNodes.add baseNimNode
    result = mulNimNodes(nimNodes)
  of symPow:
    let base = newLit compileSymNode(symNode.children[0])
    let exponent = newLit compileSymNode(symNode.children[1])
    result = quote do:
      #`base` ^ `exponent`
      pow(`base`, `exponent`)
  of symFunc:
    # something like symFuncCompileProcs[symNode.funcName](symNode)
    assert isValidFunc(symNode.funcName)
    result = compileProcsCT[symNode.funcName](symNode)

macro compile*(symNode: static SymNode): untyped =
  result = compileSymNode(symNode)
  echo result.repr

macro generate*(symObj: typed, signatures: untyped): untyped =
  result = newStmtList()
  signatures.expectKind(nnkStmtList)
  #echo signatures[0].kind
  #echo signatures.treeRepr
  for sig in signatures:
    sig.expectKind({nnkProcDef, nnkFuncDef})
    let newSig = copyNimTree(sig)
    newSig[6] = quote do:
      compile(`symObj`)
    result.add newSig
  echo result.repr