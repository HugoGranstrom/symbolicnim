import rationals, math, tables, sequtils
import ./ast_types, ./utils

### Forward declarations
proc `*`*(a, b: SymNode): SymNode
proc `+`*(a, b: SymNode): SymNode


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

proc `-`*(a: SymNode): SymNode =
  newSymNumber(-1 // 1) * a

proc diff_internal*(symNode: SymNode, dVar: SymNode): SymNode =
  assert dVar.kind == symSymbol, "You can only take the derivative with respect to a symbol!"
  case symNode.kind
  of symSymbol:
    if symNode.name == dVar.name: return newSymNumber(1 // 1)
    return newSymNumber(0 // 1)
  of symNumber:
    return newSymNumber(0 // 1)
  of symAdd:
    result = newSymNumber(0 // 1)
    for term, coeff in pairs(symNode.terms):
      result = result + newSymNumber(coeff) * diff_internal(term, dVar)
  of symMul:
    result = newSymNumber(0 // 1)
    let pairs = toSeq(pairs(symNode.products))
    for i in 0 .. pairs.high:
      var temp = newSymNumber(symNode.coeff)
      for j in 0 .. pairs.high:
        if i == j:
          temp = temp * diff_internal(pairs[j][0] ^ pairs[j][1], dVar)
        else:
          temp = temp * pairs[j][0] ^ pairs[j][1]
      result = result + temp
  of symPow:
    discard
  of symFunc:
    discard # something like symFuncDiffProcs[symNode.funcName](symNode)