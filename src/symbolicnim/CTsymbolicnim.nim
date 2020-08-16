import tables, rationals, macros, hashes, sequtils, math

type
  SymNodeKind* = enum
    symSymbol # "variables"
    symNumber # literal
    symAdd
    symMul
    symPow
    symCondition # branches. Can we parse an if-statement to this?

  SymNode* = ref object
    children*: seq[SymNode]
    hashCache*: Hash
    case kind*: SymNodeKind
    of symSymbol:
      name*: string
    of symNumber:
      lit*: Rational[int]
    of symAdd:
      terms: Table[SymNode, Rational[int]]
      constant: Rational[int]
    of symMul:
      products: Table[SymNode, SymNode] # Table[base, exponent]
      coeff: Rational[int]
    else:
      discard

# Forward Declarations
proc `+`*(a, b: SymNode): SymNode
proc `*`*(a, b: SymNode): SymNode


proc rationalToString*(r: Rational[int]): string =
  if r.den == 1:
    return $r.num
  return $r

proc isInteger*(r: Rational[int]): bool =
  r.den == 1

proc pow*(r: Rational[int], e: int): Rational[int] =
    if e < 0:
        result.num = r.den ^ (-e)
        result.den = r.num ^ (-e)
    else:
        result.num = r.num ^ e
        result.den = r.den ^ e

proc `$`*(symNode: SymNode): string =
  case symNode.kind
  of symSymbol: return symNode.name
  of symNumber: return rationalToString(symNode.lit)
  #of symAdd, symMul: $symNode.terms & " + " & $symNode.constant
  of symAdd:
    result = "("
    if symNode.constant != 0 // 1:
      result.add rationalToString(symNode.constant) & " + "
    for term, coeff in pairs(symNode.terms):
      if coeff != 1 // 1:
        result.add rationalToString(coeff) & "*" & $term & " + "
      else:
        result.add $term & " + "
    result = result[0 .. ^4]
    result.add ")"
  of symMul:
    result = "("
    if symNode.coeff != 1 // 1:
      result.add rationalToString(symNode.coeff) & "*"
    for base, exponent in pairs(symNode.products):
      if exponent.kind == symNumber and exponent.lit == 1 // 1:
        result.add $base & "*"
      else:
        result.add $base & "^(" & $exponent & ")*"
    result = result[0 .. ^2]
    result.add ")"
  of symPow:
    result = "(" & $symNode.children[0] & "^" & $symNode.children[1] & ")"
  else: return $symNode.children

proc hash*(symNode: SymNode): Hash =
  # Check if hash is cached
  if symNode.hashCache != 0:
    return symNode.hashCache
  # Otherwise calculate and cache it
  result = result !& hash(symNode.children)
  result = result !& hash(symNode.kind)
  case symNode.kind:
    of symSymbol: result = result !& hash(symNode.name)
    of symNumber: result = result !& hash(symNode.lit)
    of symAdd: result = result !& hash($symNode.terms) !& hash(symNode.constant)
    of symMul: result = result !& hash($symNode.products) !& hash(symNode.coeff)
    else: discard
  result = !$result
  symNode.hashCache = result
  #return result

proc `==`*(a, b: SymNode): bool =
  hash(a) == hash(b) # This works assuming we have a good hash algorithm
  #a[] == b[]

proc newSymNode*(kind: SymNodeKind): SymNode =
  result = SymNode(kind: kind)
  if kind == symMul: result.coeff = 1 // 1
  elif kind == symAdd: result.constant = 0 // 1
  elif kind == symNumber: result.lit = 0 // 1 # by default 0 // 0 which isn't good

proc newSymbol*(name: string): SymNode =
  result = newSymNode(symSymbol)
  result.name = name

proc newSymNumber*(lit: Rational[int]): SymNode =
  result = newSymNode(symNumber)
  result.lit = lit


proc mulToKey*(mul: SymNode): SymNode =
  assert mul.kind == symMul
  if mul.products.len == 1: # 2*x, return x as symbol not {x: 1}
    result = toSeq(keys(mul.products))[0]
  else:
    result = newSymNode(symMul)
    result.products = mul.products
  



proc `^`*(a, b: SymNode): SymNode =
  result = newSymNode(symPow)
  result.children.add a
  result.children.add b # these will persist if no simplification is found
  if b.kind == symNumber:
    if b.lit == 0 // 1: # a ^ 0 = 1
      return newSymNumber(1 // 1)
    elif b.lit == 1 // 1: # a ^ 1 = a
      return a
  if a.kind == symNumber:
    if a.lit == 0 // 1: # 0 ^ b = 0
      return newSymNumber(0 // 1)
    elif a.lit == 1 // 1: # 1 ^ b = 1
      return newSymNumber(1 // 1)
  if a.kind == symPow: # (x ^ y) ^ b = x ^ (y*b)
    result = a.children[0] ^ (a.children[1] * b)
  elif a.kind == symMul: # (x*y)^b = x^b * y^b. (2*x)^b = 2^b * x^b * 1 (if b != symNumber, move coeff into table)
    # (x^2*y^3)^b = x^(2*b) * y^(3*b)
    result = newSymNode(symMul) 
    if b.kind == symNumber and isInteger(b.lit): # coeff can keep it's place, just take it to the power of b.lit. b.lit must be integer though.
      result.coeff = pow(a.coeff, b.lit.num)
    else: # coeff must move out and we must set coeff = 1//1 again.
      result.products[newSymNumber(a.coeff)] = b
    for base, exponent in pairs(a.products):
      let newExponent = exponent * b
      if newExponent.kind == symNumber and newExponent.lit != 0 // 1:
        result.products[base] = newExponent
    if result.products.len == 0: # 2*{} = 2
      return newSymNumber(result.coeff)
    elif result.products.len == 1 and result.coeff == 1 // 1: # 1*x^y = x^y
      let base = toSeq(keys(result.products))[0]
      let exponent = toSeq(values(result.products))[0]
      return base ^ exponent
  

proc `*`*(a, b: SymNode): SymNode =
  result = newSymNode(symMul)
  let aIsMul = a.kind == symMul
  let bIsMul = b.kind == symMul
  if aIsMul and bIsMul:
    let newConstant = a.coeff * b.coeff
    if newConstant == 0 // 1:
      return newSymNumber(0 // 1)
    result.coeff = newConstant
    for base, exponent in pairs(a.products):
      if base in b.products:
        let newExponent = exponent + b.products[base]
        if newExponent.kind == symNumber and newExponent.lit != 0 // 1:
          result.products[base] = newExponent
      else:
        result.products[base] = exponent
    for base, exponent in pairs(b.products):
      if base notin a.products:
        result.products[base] = exponent
  elif aIsMul or bIsMul:
    var singleNode, mulNode: SymNode
    if aIsMul:
      mulNode = a
      singleNode = b
    else:
      mulNode = b
      singleNode = a
    result.products = mulNode.products
    result.coeff = mulNode.coeff
    if singleNode.kind == symPow:
      let singleBase = singleNode.children[0]
      let singleExponent = singleNode.children[1]
      if singleBase in result.products:
        let newExponent = singleExponent + result.products[singleBase]
        if newExponent.kind == symNumber and newExponent.lit == 0 // 1:
          result.products.del(singleBase)
        else:
          result.products[singleBase] = newExponent
      else:
        result.products[singleBase] = singleExponent
    elif singleNode.kind == symNumber:
      if singleNode.lit == 0 // 1:
        return newSymNumber(0 // 1)
      result.coeff *= singleNode.lit
    elif singleNode in result.products:
      let newExponent = result.products[singleNode] + newSymNumber(1 // 1)
      if newExponent.kind == symNumber and newExponent.lit == 0 // 1:
        result.products.del(singleNode)
      else:
        result.products[singleNode] = newExponent
    else:
      result.products[singleNode] = newSymNumber(1 // 1)
  else:
    let aIsPow = a.kind == symPow
    let bIsPow = b.kind == symPow
    if aIsPow and bIsPow:
      let aBase = a.children[0]
      let aExponent = a.children[1]
      let bBase = b.children[0]
      let bExponent = b.children[1]
      if aBase == bBase:
        result = aBase ^ (aExponent + bExponent)
      else:
        result.products[aBase] = aExponent
        result.products[bBase] = bExponent
    elif aIsPow or bIsPow:
      var singleNode, powNode: SymNode
      if aIsPow:
        powNode = a
        singleNode = b
      else:
        powNode = b
        singleNode = a
      let base = powNode.children[0]
      let exponent = powNode.children[1]
      if singleNode.kind == symNumber:
        if singleNode.lit == 0 // 1:
          return newSymNumber(0 // 1)
        result.coeff = singleNode.lit
        result.products[base] = exponent
      elif singleNode == base:
        let newExponent = exponent + newSymNumber(1 // 1)
        result = base ^ newExponent # should handle case when newExponent = 0
      else:
        result.products[base] = exponent
        result.products[singleNode] = newSymNumber(1 // 1)
    else:
      if a == b:
        return a ^ newSymNumber(2 // 1)
      if a.kind == symNumber:
        result.coeff *= a.lit
      else:
        result.products[a] = newSymNumber(1 // 1)
      if b.kind == symNumber:
        result.coeff *= b.lit
      else:
        result.products[b] = newSymNumber(1 // 1)
      if result.coeff == 0 // 1:
        return newSymNumber(0 // 1)
  if result.kind == symMul and result.products.len == 0:
    return newSymNumber(result.coeff)
  elif result.kind == symMul and result.products.len == 1 and result.coeff == 1 // 1: # 1*x^y = x^y
    let base = toSeq(keys(result.products))[0]
    let exponent = toSeq(values(result.products))[0]
    return base ^ exponent # this should handle the case when coeff = 1 either way.

      

proc `/`*(a, b: SymNode): SymNode =
  a * (b ^ newSymNumber(-1 // 1))

proc `+`*(a, b: SymNode): SymNode =
  result = newSymNode(symAdd)
  let aIsAdd = a.kind == symAdd
  let bIsAdd = b.kind == symAdd
  if aIsAdd and bIsAdd:
    result.constant = a.constant + b.constant
    for term, coeff in pairs(a.terms):
      if term in b.terms:
        let newCoeff = coeff + b.terms[term]
        if newCoeff != 0 // 1: # 0 * () should be removed
          result.terms[term] = newCoeff
      else:
        result.terms[term] = coeff
    for term, coeff in pairs(b.terms):
      if term notin a.terms: # don't double count
        result.terms[term] = coeff
  elif aIsAdd or bIsAdd:
    var addNode: SymNode
    var singleNode: SymNode # can we do this for case objects? We should as we assign the entire object.
    if aIsAdd:
      addNode = a
      singleNode = b
    else:
      addNode = b
      singleNode = a
    result.terms = addNode.terms
    result.constant = addNode.constant
    # Must check if singleNode is Mul and if so handle constant correctly. Check if the singleNode.terms hash is in addNode.terms. We don't want to have the constant in this hash. (x*z + 3*y) + (2*x*z) (x*z) is the key we identify terms by.
    # Does current hash function work for this? hash(x*z) is same as hash(x*z + 0)? x*z = {x: 1, z: 1}. We want hash(x*z) == hash((x*z).terms) when constant is zero. Children and kind makes trouble though.
    # So only add hash(constant) when constant != 0 // 1
    # What exactly is the keys of 2*x*y + 3*y + 5 -> {x*y: 2, y: 3} + 5
    # What is x*y? {x: 1, y: 1} * 1
    # So for it to work, x*y must be represented they same in both cases.
    # Verify that they match.
    if singleNode.kind == symMul:
      # we must reset the constant of Mul to 1 for it to match the key in Add!!!!! Make a copy and set constant = 1. Then use it as the key!
      let singleKey = mulToKey(singleNode)
      if singleKey in result.terms:
        let newCoeff = result.terms[singleKey] + singleNode.coeff
        if newCoeff != 0 // 1:
          result.terms[singleKey] = newCoeff
        else:
          result.terms.del(singleKey)
      else:
        result.terms[singleKey] = singleNode.coeff
    elif singleNode in result.terms:
      let newCoeff = result.terms[singleNode] + 1 // 1
      if newCoeff != 0 // 1:
        result.terms[singleNode] = newCoeff
      else: # if zero, remove it from sum
        result.terms.del(singleNode)
    elif singleNode.kind == symNumber: # add it to constant
      result.constant += singleNode.lit
    else: # if it doesn't exist yet, add it
      result.terms[singleNode] = 1 // 1
  else: # neither a nor b is Add.
    # do we have to take care of the case Mul + Mul? Yes because we need to handle 2*() + 3*() = 5*()
    let aIsMul = a.kind == symMul
    let bIsMul = b.kind == symMul
    if aIsMul and bIsMul:
      let aKey = mulToKey(a)
      let bKey = mulToKey(b)
      if aKey == bKey:
        let newConst = a.coeff + b.coeff
        if newConst == 0 // 1: # -1() + 1() = 0() = 0
          return newSymNumber(0 // 1)
        else: # 2x + 3x = 5x (mul)
          result = aKey
          result.coeff = newConst # we can change this because aKey isn't used anymore.
      else: # just create a Add with respective muls. Coefficent should be as high up as possible, ie in the Add
        result.terms[aKey] = a.coeff
        result.terms[bKey] = b.coeff
    elif aIsMul or bIsMul:
      # singleNode can't be add or mul!
      var singleNode, mulNode: SymNode
      if aIsMul:
        mulNode = a
        singleNode = b
      else:
        mulNode = b
        singleNode = a
      let mulCoeff = mulNode.coeff
      let mulKey = mulToKey(mulNode)
      if singleNode == mulKey:
        let newCoeff = mulCoeff + 1 // 1
        if newCoeff != 0 // 1: # this should be redundant as `*` should take care of 0 * x = 0
          result = newSymNumber(newCoeff) * mulKey
        else:
          return newSymNumber(0 // 1)
      elif singleNode.kind == symNumber:
        result.terms[mulKey] = mulCoeff
        result.constant = singleNode.lit
      else:
        result.terms[singleNode] = 1 // 1
        result.terms[mulKey] = mulCoeff
    else:
      if a == b:
        result = newSymNumber(2 // 1) * a
      else: # 2 + x comes here! and 2 + 2 for that matter
        if a.kind == symNumber:
          result.constant += a.lit
        else:
          result.terms[a] = 1 // 1
        if b.kind == symNumber:
          result.constant += b.lit
        else:
          result.terms[b] = 1 // 1
  # should this be place at top level? Probably! Along with len == 1 case.
  if result.kind == symAdd and result.terms.len == 0: # const + {}, just return constant
    return newSymNumber(result.constant)
  elif result.kind == symAdd and result.terms.len == 1 and result.constant == 0 // 1:
    let key = toSeq(keys(result.terms))[0]
    let coeff = toSeq(values(result.terms))[0]
    return key * newSymNumber(coeff) # this should handle the case when coeff = 1 either way.

proc `-`*(a, b: SymNode): SymNode =
  a + newSymNumber(-1 // 1) * b 


######
##### Backend Stuff
######

var simplifyProcsCT {.compileTime.} = newSeq[proc(node: SymNode): SymNode]()
var simplifyProcsRT = newSeq[proc(node: SymNode): SymNode]()

macro simplifyPass*(simpProc: untyped): untyped =
  simpProc.expectKind(nnkProcDef)
  let procName = name(simpProc)
  result = quote do: # just define the proc as usual and add it to simplifyProcs
    `simpProc`
    static: simplifyProcsCT.add `procName`
    simplifyProcsRT.add `procName`

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
      `base` ^ `exponent`
  else:
    discard

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

### Testing

#[
"Hej".generate:
  proc f*(x: float): auto
  proc g*(x: float): auto
  proc h*(x: float)

newSymbol("x").generate:
  proc q(x: float): auto
]#
let s{.compiletime.} = newSymNumber(2 // 3)
s.generate:
  proc r(): auto

converter intToFloat(d: int): float = d.toFloat
proc pow(d, e: int): int = d ^ e

let x {.compiletime.} = newSymbol("x")
let y {.compiletime.} = newSymbol("y")
let expr1 {.compiletime.} = x*y^newSymNumber(2 // 1)
expr1.generate:
  proc f1(x, y: int): int
echo f1(2, 3)
#[
when isMainModule:
  converter intToSymNumber(d: int): SymNode =
    newSymNumber(d.toRational)
  #[var t = initTable[string, int]()
  t["hej"] = 1
  t["då"] = 2
  t["re"] = 3
  echo t
  var a = initTable[string, int]()
  a["då"] = 2
  a["re"] = 3
  a["hej"] = 1
  echo a]#
  let x = newSymbol("x")
  let y = newSymbol("y")
  echo hash(x)
  echo x == newSymbol("x")
  f(1.0)
  g(1.0)
  h(1.0)
  echo q(7.8)
  echo r()
  echo x + y
  echo x + newSymNumber(2 // 1) + newSymNumber(5 // 1) + y
  echo x*y
  echo newSymNumber(2 // 1)*x + y + x + y - x
  echo 2*(2*x)
  echo 2*x^2 * x
  echo (x*y) * x^2
  echo (x*y + 7)
  echo x*x^2 * 2 * y * x^3
  echo x*x^2 * 2 * y * x^3 + x^3 * 2 * y * x*x^2
  echo x^2 * y / (x*y) 
  echo 0*(x^y)
  echo (x*y*2) ^ newSymNumber(3 // 2)
  echo (2 ^ x ^ y) ^ 5
  echo (x^y)*5
]#