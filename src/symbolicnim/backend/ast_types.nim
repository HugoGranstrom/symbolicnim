import tables, rationals, hashes, math, strutils
import ./utils
type
  SymNodeKind* = enum
    symSymbol # "variables"
    symNumber # literal
    symAdd
    symMul
    symPow
    symFunc

  SymNode* = ref object
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
  of symFunc:
    result = symNode.funcName & "(" & $symNode.children & ")"

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