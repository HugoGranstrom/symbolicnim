import tables, rationals, hashes, math, strutils, sequtils
import ./utils
type
  SymNodeKind* = enum
    symSymbol # "variables"
    symNumber # literal
    symAdd
    symMul
    symPow
    symFunc

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

proc `$`*(symNode: SymNode): string =
  case symNode.kind
  of symSymbol: return symNode.name
  of symNumber: return rationalToString(symNode.lit)
  #of symAdd, symMul: $symNode.terms & " + " & $symNode.constant
  of symAdd:
    if symNode.constant != 0 // 1:
      result.add rationalToString(symNode.constant) & " + "
    for term, coeff in pairs(symNode.terms):
      if coeff != 1 // 1:
        result.add rationalToString(coeff) & "*" & $term & " + "
      else:
        result.add $term & " + "
    result = result[0 .. ^4]
  of symMul:
    if symNode.coeff != 1 // 1:
      result.add rationalToString(symNode.coeff) & "*"
    for base, exponent in pairs(symNode.products):
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

# We need a new hash function that reuses hashes!!!
# because symAdd and symMUl uses `$` it doesn't reuse any of the children's hashes!

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
      result = result !& hash(toSeq(pairs(symNode.terms))) !& hash(symNode.constant)
    of symMul:
      #result = result !& hash($symNode.products) !& hash(symNode.coeff)
      result = result !& hash(toSeq(pairs(symNode.products))) !& hash(symNode.coeff)
    of symFunc: result = result !& hash(symNode.funcName) !& hash(symNode.nargs)
    of symPow: discard
  result = !$result
  symNode.hashCache = result

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

proc `==`*(a, b: SymNode): bool =
  hash(a) == hash(b) # This works assuming we have a good hash algorithm
  #a[] == b[]

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