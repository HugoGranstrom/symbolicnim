import tables, rationals, hashes, math, strutils, sequtils, algorithm
import ./utils
type
  SymNodeKind* = enum
    symNumber # literal
    symSymbol # "variables"
    symPow
    symFunc
    symAdd
    symMul
    

  SymNode* = ref object # {.acyclic.}?
    children*: seq[SymNode]
    hashCache*: Hash
    case kind*: SymNodeKind
    of symSymbol:
      name*: string
    of symNumber:
      lit*: Rational[int]
    of symAdd:
      terms*: Table[SymNode, Rational[int]]
      constant*: Rational[int]
    of symMul:
      products*: Table[SymNode, SymNode] # Table[base, exponent]
      coeff*: Rational[int]
    of symFunc:
      funcName*: string
      nargs*: int
    of symPow:
      discard

proc symNodeCmp*(x, y: SymNode): int
proc symNodeCmpTuple1*(x, y: tuple[key: SymNode, value: Rational[int]]): int
proc symNodeCmpTuple2*(x, y: tuple[key: SymNode, value: SymNode]): int



# We need a new hash function that reuses hashes!!!
# because symAdd and symMUl uses `$` it doesn't reuse any of the children's hashes!

# We need to sort pairs! If kind1 != kind2 sort! If same, sort accordinly. pairs doesn't garanti that we will always get the items in the same order.

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
    of symAdd:
      # loop over pairs and add hash(coeff) !& hash(term)
      #result = result !& hash($symNode.terms) !& hash(symNode.constant)
      result = result !& hash(symNode.constant)
      var pairsSeq = toSeq(pairs(symNode.terms))
      pairsSeq.sort(symNodeCmpTuple1)
      result = result !& hash(pairsSeq)
    of symMul:
      #result = result !& hash($symNode.products) !& hash(symNode.coeff)
      result = result !& hash(symNode.coeff)
      var pairsSeq = toSeq(pairs(symNode.products))
      pairsSeq.sort(symNodeCmpTuple2)
      result = result !& hash(pairsSeq)
    of symFunc: result = result !& hash(symNode.funcName) !& hash(symNode.nargs)
    of symPow: discard
  result = !$result
  symNode.hashCache = result

#[
proc hashOld*(symNode: SymNode): Hash =
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
    of symFunc: result = result !& hash(symNode.funcName) !& hash(symNode.nargs)
    of symPow: discard
  result = !$result
  symNode.hashCache = result
  #return result
]#
proc `==`*(a, b: SymNode): bool =
  hash(a) == hash(b) # This works assuming we have a good hash algorithm
  #a[] == b[]

proc `<`*(a, b: SymNode): bool =
  if ord(a.kind) < ord(b.kind):
    return true
  elif a.kind > b.kind:
    return false
  # kind is the same, check their elements
  case a.kind
  of symNumber:
    return a.lit < b.lit
  of symSymbol:
    return a.name < b.name
  of symFunc:
    if a.funcName >= b.funcName: return false
    for (aChild, bChild) in zip(a.children, b.children):
      if aChild >= bChild: return false
    return true
  of symPow:
    return a.children[0] < b.children[0] and a.children[1] < b.children[1]
  of symAdd:
    if a.constant >= b.constant or a.terms.len >= b.terms.len: return false
    for (aKey, bKey) in zip(toSeq keys(a.terms), toSeq keys(b.terms)):
      if aKey >= bKey: return false
    for (aCoeff, bCoeff) in zip(toSeq values(a.terms), toSeq values(b.terms)):
      if aCoeff >= bCoeff: return false
    return true
  of symMul:
    if a.coeff >= b.coeff or a.products.len >= b.products.len: return false
    for (aKey, bKey) in zip(toSeq keys(a.products), toSeq keys(b.products)):
      if aKey >= bKey: return false
    for (aVal, bVal) in zip(toSeq values(a.products), toSeq values(b.products)):
      if aVal >= bVal: return false
    return true

proc symNodeCmp*(x, y: SymNode): int =
  if x == y: return 0
  if x < y: return -1
  return 1

proc symNodeCmpTuple1*(x, y: tuple[key: SymNode, value: Rational[int]]): int =
  if x[0] == y[0] and x[1] == y[1]: return 0
  if x[0] < y[0]: return -1 # we only sort on the "term", not the coeff, as the term will be unique
  #elif x[1] < y[1]: echo "-1";return -1
  return 1

proc symNodeCmpTuple2*(x, y: tuple[key: SymNode, value: SymNode]): int =
  if x[0] == y[0] and x[1] == y[1]: return 0
  if x[0] < y[0]: return -1 # we only sort on the "base", not the exponent, as the base will be unique
  #and x[1] < y[1]
  return 1

proc `$`*(symNode: SymNode): string =
  case symNode.kind
  of symSymbol: return symNode.name
  of symNumber: return rationalToString(symNode.lit)
  #of symAdd, symMul: $symNode.terms & " + " & $symNode.constant
  of symAdd:
    if symNode.constant != 0 // 1:
      result.add rationalToString(symNode.constant) & " + "
    var pairsSeq = toSeq pairs(symNode.terms)
    pairsSeq.sort(symNodeCmpTuple1)
    for (term, coeff) in pairsSeq:
      if coeff != 1 // 1:
        result.add rationalToString(coeff) & "*" & $term & " + "
      else:
        result.add $term & " + "
    result = result[0 .. ^4]
  of symMul:
    if symNode.coeff != 1 // 1:
      result.add rationalToString(symNode.coeff) & "*"
    var pairsSeq = toSeq pairs(symNode.products)
    pairsSeq.sort(symNodeCmpTuple2)
    for (base, exponent) in pairsSeq:
      var baseStr: string
      if base.kind in {symAdd, symMul, symPow}:
        baseStr = "(" & $base & ")"
      else:
        baseStr = $base
      if exponent.kind == symNumber and exponent.lit == 1 // 1:
        result.add baseStr & "*"
      else:
        var exponentStr: string
        if exponent.kind in {symAdd, symMul, symPow}:
          exponentStr = "(" & $exponent & ")"
        else:
          exponentStr = $exponent
        result.add baseStr & "^" & exponentStr & "*"
    result = result[0 .. ^2]
  of symPow:
    let base = symNode.children[0]
    let exponent = symNode.children[1]
    var baseStr, exponentStr: string
    if base.kind in {symAdd, symMul, symPow}:
      baseStr = "(" & $base & ")"
    else:
      baseStr = $base
    if exponent.kind in {symAdd, symMul, symPow}:
      exponentStr = "(" & $exponent & ")"
    else:
      exponentStr = $exponent
    result = baseStr & "^" & exponentStr
  of symFunc:
    var argStr: string
    for child in symNode.children:
      argstr.add $child & ", "
    argStr = argStr[0 .. ^3]
    result = symNode.funcName & "(" & argStr & ")"

proc copySymNode*(symNode: SymNode): SymNode =
  # shallow copy (just underlying SymNode)
  discard

proc copySymTree*(symNode: SymNode): SymNode =
  # deep copy (all decendent nodes)
  discard

proc newSymNode*(kind: SymNodeKind): SymNode =
  result = SymNode(kind: kind)
  if kind == symMul: result.coeff = 1 // 1
  elif kind == symAdd: result.constant = 0 // 1
  elif kind == symNumber: result.lit = 0 // 1 # by default 0 // 0 which isn't good

proc newSymbolNode*(name: string): SymNode {.raises:[ValueError].} =
  if " " in name: raise newException(ValueError, "Symbol name '" & name & "' contains a space!")
  result = newSymNode(symSymbol)
  result.name = name

proc newSymNumber*(lit: Rational[int]): SymNode =
  result = newSymNode(symNumber)
  result.lit = lit