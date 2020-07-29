import sets, hashes, strformat, sequtils, rationals, algorithm, tables
import math
export sets, rationals
type
    SymNumberType* = Rational[int]

    SymbolicVariable* = ref object
        name: string
    
    FuncCallKind* = enum
        expFunc,
        lnFunc,
        sinFunc,
        cosFunc,
        tanFunc

    ExprKind* = enum
        exprConstant,
        exprVariable,
        exprFuncCall,
        exprExponent,
        exprTerms,
        exprFactors,

    SymbolicExpression* = object
        deps: HashSet[SymbolicVariable]
        children: seq[SymbolicExpression]
        case kind: ExprKind
        of exprVariable:
            name: string
        of exprConstant:
            value: SymNumberType
        of exprFuncCall:
            funcKind: FuncCallKind
        else:
            discard 
    
    #SymbolicExpression* = object
    #    deps*: HashSet[SymbolicVariable]
    #    treeRepr*: SymbolicExpression

    SurveyObject = Table[ExprKind, tuple[count: int, indexes: seq[int]]] #object # Table[]
        #constants, variables, terms, factors, exponents: tuple[count: int, index: seq[int]]
        #content: Table[ExprKind, tuple[count: int, index: seq[int]]]

#### Forward declarations
proc constructTerms(terms: seq[SymbolicExpression]): SymbolicExpression
proc constructFactors(factors: seq[SymbolicExpression]): SymbolicExpression
proc constructExponent*(base, exponent: SymbolicExpression): SymbolicExpression
proc prettyString(tree: SymbolicExpression): string
proc diff_internal(dVar: SymbolicVariable, tree: SymbolicExpression): SymbolicExpression
#### Custom procs

proc pop[T](list: var seq[T], index: int): T =
    result = list[index]
    list.delete(index)

proc pop[T](list: var seq[T], indexes: seq[int]): seq[T] =
    discard # indexes must be sorted. account for every time we remove a element

proc isInteger(r: SymNumberType): bool =
    if r.den == 1: return true
    return false

proc isHalfMultiple(r: SymNumberType): bool =
    if r.den == 2 or r.den == 1: return true
    return false

proc pow*(r: SymNumberType, e: int): SymNumberType =
    if e < 0:
        result.num = r.den ^ (-e)
        result.den = r.num ^ (-e)
    else:
        result.num = r.num ^ e
        result.den = r.den ^ e

#### SymbolicVariable
const pi_str = "π"
const reserved_var_names* = ["π", "sym_e", "sym_i", "sym_inf", "sym_true", "sym_false"]
let sym_pi* = SymbolicVariable(name: "π")
let sym_e* = SymbolicVariable(name: "sym_e")
let sym_i* = SymbolicVariable(name: "sym_i")
let sym_inf* = SymbolicVariable(name: "sym_inf")

proc newVariable*(name: string): SymbolicVariable {.raises: [ValueError].} =
    if name in reserved_var_names:
        raise newException(ValueError, &"Variable name '{name}' is reserved, use the predefined variable exported as '{name}' instead")
    result = SymbolicVariable(name: name)

proc hash*(symVar: SymbolicVariable): Hash =
    symVar.name.hash

proc `==`*(symVar1, symVar2: SymbolicVariable): bool =
    symVar1.name == symVar2.name

proc `$`*(symVar: SymbolicVariable): string =
    symVar.name

converter symVarToSymExpr*(symVar: SymbolicVariable): SymbolicExpression =
    SymbolicExpression(kind: exprVariable, deps: [symVar].toHashSet, name: symVar.name)

converter rationalToSymExpr*(d: SymNumberType): SymbolicExpression =
    result = SymbolicExpression(kind: exprConstant, value: d)

converter someNumberToSymExpr*[T: SomeNumber](d: T): SymbolicExpression =
    SymbolicExpression(kind: exprConstant, value: d.toRational)

#### SymbolicExpression
proc initExprConstant(value: SymNumberType): SymbolicExpression =
    SymbolicExpression(kind: exprConstant, value: value)

proc initExprVariable(name: string): SymbolicExpression =
    SymbolicExpression(kind: exprVariable, name: name)

proc initExprTerms(): SymbolicExpression =
    SymbolicExpression(kind: exprTerms)

proc initExprFactors(): SymbolicExpression =
    SymbolicExpression(kind: exprFactors)

proc initExprExponent(): SymbolicExpression =
    SymbolicExpression(kind: exprExponent)

proc initExprFuncCall(funcKind: FuncCallKind): SymbolicExpression =
    SymbolicExpression(kind: exprFuncCall, funcKind: funcKind)
    

# Where should we sort?
# Customize loops to account for the fact it's sorted.
# We can't assume == can just use children1 == children2 because we don't sort the order in the types themself. could be 1/2 , 5/7 or the other way
# survey should be able to use this though.
proc cmp(x, y: SymbolicExpression): int =
    let xOrd = ord(x.kind)
    let yOrd = ord(y.kind)
    if xOrd == yOrd: return 0
    if xOrd < yOrd: return -1
    return 1

proc equal*(tree1, tree2: SymbolicExpression): bool =
    if tree1.kind == tree2.kind:
        case tree1.kind
        of exprConstant:
            if tree1.value != tree2.value: return false
        of exprVariable:
            if tree1.name != tree2.name: return false
        of exprFuncCall:
            if tree1.funcKind != tree2.funcKind: return false
        else:
            discard
        if tree1.children.len != tree2.children.len: return false
        # Now it means tree1 has same length as tree2
        if tree1.deps != tree2.deps: return false
        if tree1.kind == exprFactors or tree1.kind == exprTerms:
            # We must take into account that a * b * c is equal to b * c * a or any other permutation. 
            var available_indexes: seq[int] = toSeq(0 .. tree1.children.high)
            # iterate over one of the trees and compare it to each of the other tree's children.
            # if no match is found, return false. if match is found, remove that index from available_indexes and break.
            var found_match = false
            for i in 0 .. tree1.children.high:
                found_match = false
                for j in available_indexes:
                    if tree1.children[i].equal(tree2.children[j]):
                        found_match = true
                        available_indexes.delete(available_indexes.find(j))
                        break
                if not found_match: return false
        else:
            #if tree1.children != tree2.children: return false
            for i in 0 .. tree1.children.high:
                if not equal(tree1.children[i], tree2.children[i]): return false
        return true
    else:
        return false

proc contains*(tree: SymbolicExpression, kind: ExprKind): bool {.noSideEffect.} =
    if tree.children.len == 0:
        return false
    else:
        for child in tree.children:
            if child.kind == kind:
                return true
        return false

proc contains*(tree: SymbolicExpression, symVar: SymbolicVariable): bool {.noSideEffect.} =
    symVar in tree.deps

proc survey(trees: openArray[SymbolicExpression]): SurveyObject =
    for e in ExprKind:
        result[e] = (count: 0, indexes: newSeqOfCap[int]((trees.len / 2).toInt))
    for i in 0 .. trees.high:
        case trees[i].kind
        of exprConstant:
            inc result[exprConstant].count
            result[exprConstant].indexes.add(i)
        of exprVariable:
            inc result[exprVariable].count
            result[exprVariable].indexes.add(i)
        of exprFactors:
            inc result[exprFactors].count
            result[exprFactors].indexes.add(i)
        of exprTerms:
            inc result[exprTerms].count
            result[exprTerms].indexes.add(i)
        of exprExponent:
            inc result[exprExponent].count
            result[exprExponent].indexes.add(i)
        of exprFuncCall:
            inc result[exprFuncCall].count
            result[exprFuncCall].indexes.add(i)

template survey(tree: SymbolicExpression): SurveyObject =
    survey(tree.children)

proc prettyString(tree: SymbolicExpression): string =
    case tree.kind
    of exprConstant:
        if isInteger(tree.value): return $tree.value.num
        return $tree.value
    of exprVariable:
        return tree.name
    of exprTerms:
        for i in 0 .. tree.children.high:
            if i != 0:
                result.add " + "
            result.add prettyString(tree.children[i])
    of exprFactors:
        # if it contains exponents with negative exponents, write as a fraction
        for i in 0 .. tree.children.high:
            if i != 0:
                result.add "*"
            if tree.children[i].kind != exprTerms:
                result.add prettyString(tree.children[i])
            else:
                result.add &"({prettyString(tree.children[i])})"
    of exprExponent:
        var baseStr, exponentStr: string
        if tree.children[0].kind in [exprTerms, exprFactors] or (tree.children[0].kind == exprConstant and not isInteger(tree.children[0].value)):
            baseStr = &"({prettyString(tree.children[0])})"
        else:
            baseStr = prettyString(tree.children[0])
        if tree.children[1].kind in [exprTerms, exprFactors] or (tree.children[1].kind == exprConstant and not isInteger(tree.children[1].value)):
            exponentStr = &"({prettyString(tree.children[1])})"
        else:
            exponentStr = prettyString(tree.children[1])
        result = &"{baseStr}^{exponentStr}"
    of exprFuncCall:
        result = &"{($tree.funcKind)[0..^5]}({prettyString(tree.children[0])})"

proc contains*(symExprs: openArray[SymbolicExpression], kind: ExprKind): bool =
    for i in 0 .. symExprs.high:
        if kind in symExprs[i]: return true
    return false

proc simplifyConstantMul(result_factors: var seq[SymbolicExpression]) =
    ## collects all constants into one constant.
    ## Ex: 1 * 2 * x * 3 -> 6 * x
    ## Special case: 1 * x -> x
    result_factors.sort(cmp)
    let survey = survey(result_factors)
    if survey[exprConstant].count > 0:
        # must sort before running this!
        var product = 1 // 1
        for i in survey[exprConstant].indexes:
            if result_factors[i].value == 0 // 1:
                result_factors = @[initExprConstant(0 // 1)]
            product *= result_factors[i].value
        #result_factors.keepIf(proc (x: SymbolicExpression): bool = x.kind != exprConstant)
        result_factors.delete(survey[exprConstant].indexes[0], survey[exprConstant].indexes[^1])
        if product != 1 // 1 or result_factors.len == 0:
            result_factors.insert(initExprConstant(product), 0) # we know it should go here, no need to sort after this.

proc simplifyConstantMulTerm(result_factors: var seq[SymbolicExpression]) =
    ## distribute a constant in a term.
    ## Ex: 2 * (x + y) -> 2*x + 2*y
    if result_factors.len == 2:
        let survey = survey(result_factors)
        if survey[exprConstant].count == 1 and survey[exprTerms].count == 1:
            let constant = result_factors[survey[exprConstant].indexes[0]]
            let term = result_factors[survey[exprTerms].indexes[0]]
            var terms_seq = newSeqOfCap[SymbolicExpression](term.children.len)
            for i in 0 .. term.children.high:
                let factor_seq: seq[SymbolicExpression] = @[constant, term.children[i]]
                terms_seq.add constructFactors(factor_seq)
            result_factors = @[constructTerms(terms_seq)]

proc simplifySameBaseExponents(result_factors: var seq[SymbolicExpression]) =
    let survey = survey(result_factors)
    var avail_indices = toSeq(0 .. result_factors.high) # use a hash-something for this perhaps?
    var new_factors = newSeqOfCap[SymbolicExpression](result_factors.len)
    for i in 0 .. result_factors.high:
        if i in avail_indices:
            let firstFactor = result_factors[i]
            let firstKind = firstFactor.kind
            var base: SymbolicExpression
            var exponents: seq[SymbolicExpression]
            if firstKind == exprExponent:
                base = firstFactor.children[0]
                exponents = @[firstFactor.children[1]]
            elif firstKind == exprConstant:
                new_factors.add firstFactor
                avail_indices.delete(avail_indices.find(i))
                continue
            else:
                base = firstFactor
                exponents = @[someNumberToSymExpr(1)]
            avail_indices.delete(avail_indices.find(i))
            var delete_indices: seq[int]
            for j in avail_indices:
                if j in survey[exprExponent].indexes:
                    let secondBase = result_factors[j].children[0]
                    let secondExponent = result_factors[j].children[1]
                    if base.equal(secondBase):
                        exponents.add secondExponent
                        delete_indices.add j
                else:
                    let secondBase = result_factors[j]
                    if base.equal(secondBase):
                        exponents.add someNumberToSymExpr(1)
                        delete_indices.add j
            for j in delete_indices:
                avail_indices.delete(avail_indices.find(j))
            var exponent: SymbolicExpression
            if exponents.len == 1:
                exponent = exponents[0]
            else:
                exponent = constructTerms(exponents)
            let new_factor = constructExponent(base, exponent)
            new_factors.add new_factor
    result_factors = new_factors        

proc constructFactors(factors: seq[SymbolicExpression]): SymbolicExpression =
    # make this work with a seq of SymbolicExpression instead. Then put a wrapper around it that converts varargs to seq and extracts the treeRepr that is then passed to here.
    # then we can collect a seq of symTrees and pass them here, for example when we calculate the final exponent.
    var result_factors = newSeqOfCap[SymbolicExpression](factors.len)
    var survey = survey(factors)
    if survey[exprFactors].count != 0:
        for i in survey[exprFactors].indexes:
            result_factors.add(factors[i].children)
        for i in 0 .. factors.high:
            if not (i in survey[exprFactors].indexes):
                result_factors.add(factors[i])
    else:
        for i in 0 .. factors.high:
            result_factors.add factors[i]


    simplifyConstantMul(result_factors)
    
    # do same thing as for terms but for a*a -> a ** 2 and a ** 3 * a ** 5 -> a ** 8
    simplifySameBaseExponents(result_factors)

    # If result_factors.len == 2: check if it's on the form constant * terms, if so, expand!
    simplifyConstantMulTerm(result_factors)


    # Check if there are two equal statements a, rewrite to a ^ 2 then. Is this doable in a more general case?
    # create a datastructure that keeps count of the indices.
    # only have to look at 1. same kinds as it self. 2. exponents (simple as a == b.children[0])
    # check first element. getBase(list[0]). for in the rest of same kind. check if they are equal. If so, add 1 to exponent. Then check against exponents. if base is same as before, add exponent to it.
    
    # do more simplifactions here!
    simplifyConstantMul(result_factors)
    if result_factors.len == 1:
        let only_factor = result_factors[0]
        return only_factor
    var deps = result_factors[0].deps
    for i in 1 .. result_factors.high:
        deps = deps + result_factors[i].deps
    result = initExprFactors()
    result.deps = deps
    result_factors.sort(cmp)
    result.children = result_factors

#[proc constructFactorsFromExpr*(factors: varargs[SymbolicExpression]): SymbolicExpression =
    var result_factors = newSeq[SymbolicExpression](factors.len)
    for i in 0 .. factors.high:
        result_factors[i] = factors[i].treeRepr
    let result_tree = constructFactors(result_factors)
    result.deps = result_tree.deps
    result.treeRepr = result_tree
    ]#


proc simplifyConstantAdd(result_terms: var seq[SymbolicExpression]) =
    let survey = survey(result_terms)
    if survey[exprConstant].count > 1: # This should be done last!
        # we have multiple constants that can be fused. if sum is 0, remove it.
        var result_constant = 0 // 1
        for i in survey[exprConstant].indexes:
            result_constant += result_terms[i].value
        result_terms.keepIf(proc(x: SymbolicExpression): bool = x.kind != exprConstant) # remove all exprConstant
        if result_constant != 0 // 1:
            let newConstant = initExprConstant(result_constant)
            result_terms.add(newConstant)
    elif survey[exprConstant].count == 1:
        let i = survey[exprConstant].indexes[0]
        if result_terms[i].value == 0 // 1:
            result_terms.delete(i)


proc separateConstantFromFactors(factor: SymbolicExpression): tuple[constant: SymNumberType, bare_factor: SymbolicExpression] =
    assert factor.kind == exprFactors, &"factor must be of kind exprFactors but was of {factor.kind}"
    # assume all factors are sorted!
    #echo "---------------1"
    let survey = survey(factor)
    if survey[exprConstant].count == 0:
        return (constant: 1 // 1, bare_factor: factor)
    let lowIndex = survey[exprConstant].indexes[0]
    let highIndex = survey[exprConstant].indexes[^1]
    var constant = 1 // 1
    for i in lowIndex .. highIndex:
        constant *= factor.children[i].value
    if factor.children.len - survey[exprConstant].count == 1:
        return (constant: constant, bare_factor: factor.children[^1])

    var base = initExprFactors()
    
    base.children = factor.children[highIndex + 1 .. ^1]
    base.deps = factor.deps
    
    result = (constant: constant, bare_factor: base)
    #echo factor.prettyString
    #echo constant, " ", base.prettyString
    #echo "---------------2"

proc simplifyConstantMultipleTerms(result_terms: var seq[SymbolicExpression]) =
    #result_terms.sort(cmp)
    let survey = survey(result_terms)
    var avail_indices = toSeq(0 .. result_terms.high)
    var new_terms = newSeqOfCap[SymbolicExpression](result_terms.len)
    for i in 0 .. result_terms.high:
        if i in avail_indices:
            let firstTerm = result_terms[i]
            let firstKind = firstTerm.kind
            var base: SymbolicExpression
            var constant_factor: SymNumberType
            if firstKind == exprFactors:
                # remove constant
                let survey = survey(firstTerm)
                if survey[exprConstant].count == 0:
                    base = firstTerm
                    constant_factor = 1 // 1
                else:
                    let (constant, bare_factor) = separateConstantFromFactors(firstTerm)
                    base = bare_factor
                    constant_factor = constant
            elif firstKind == exprConstant:
                new_terms.add firstTerm
                avail_indices.delete(avail_indices.find(i))
                continue
            else:
                base = firstTerm
                constant_factor = 1 // 1
            avail_indices.delete(avail_indices.find(i))
            #echo "Base: ", base.prettyString
            var matches = 0

            if base.kind != exprFactors:
                # check against all of same kind, that is in avail_indices
                for j in survey[base.kind].indexes:
                    if j in avail_indices:
                        #echo "Andra: ", result_terms[j].prettyString
                        #echo base == result_terms[j]
                        if base.equal(result_terms[j]):
                            constant_factor += 1 // 1
                            avail_indices.delete(avail_indices.find(j))
                            matches += 1
            # always check against all factors in avail_indices
            for j in survey[exprFactors].indexes:
                if j in avail_indices:
                    let (constant, bare_factor) = separateConstantFromFactors(result_terms[j])
                    #echo "Andra: ", result_terms[j].prettyString
                    if base.equal(bare_factor):
                        constant_factor += constant
                        avail_indices.delete(avail_indices.find(j))
                        matches += 1
            #echo "Factor: ", constant_factor
            if constant_factor.den != 0:
                if constant_factor == 1 // 1:
                    #echo "1 ", prettyString(base) 
                    new_terms.add base
                else:
                    #echo constant_factor, " ", prettyString(base)
                    #var new_factor = initExprFactors()
                    #new_factor.children = @[rationalToSymTree(constant_factor), base] # base can be a factor so we must unpack children if it is a exprFactors
                    let new_factor = constructFactors(@[rationalToSymExpr(constant_factor), base])
                    new_terms.add new_factor
    result_terms = new_terms

proc constructTerms(terms: seq[SymbolicExpression]): SymbolicExpression =
    var result_terms = newSeqOfCap[SymbolicExpression](terms.len)
    var survey = survey(terms)
    if survey[exprTerms].count != 0:
        # we have terms here! Just concat them with terms and continue with simplifications then?
        for i in survey[exprTerms].indexes:
            result_terms.add(terms[i].children)
        for i in 0 .. terms.high:
            if not (i in survey[exprTerms].indexes):
                result_terms.add(terms[i])
    else:
        for i in 0 .. terms.high:
            result_terms.add(terms[i])
    simplifyConstantMultipleTerms(result_terms)
    simplifyConstantAdd(result_terms) # do this last!
    if result_terms.len == 0:
        return initExprConstant(0 // 1)
    elif result_terms.len == 1:
        # all simplifications have left only one element. Then return it instead
        let only_term = result_terms[0]
        return only_term
    var result_deps = result_terms[0].deps
    for i in 1 .. result_terms.high:
        result_deps = result_deps + result_terms[i].deps
    result = initExprTerms()
    result.deps = result_deps
    result_terms.sort(cmp)
    result.children = result_terms

#[proc constructTermsFromExpr*(terms: varargs[SymbolicExpression]): SymbolicExpression =
    var result_terms = newSeq[SymbolicExpression](terms.len)
    for i in 0 .. terms.high:
        result_terms[i] = terms[i].treeRepr
    let result_tree = constructTerms(result_terms)
    result.deps = result_tree.deps
    result.treeRepr = result_tree
    ]#


proc constructExponent*(base, exponent: SymbolicExpression): SymbolicExpression =
    if exponent.kind == exprConstant:
        if exponent.value == 0 // 1:
            return initExprConstant(1 // 1) # This means 0 ^ 0 is evaluated as 1
        elif exponent.value == 1 // 1:
            return base
    if base.kind == exprConstant:
        if base.value == 0 // 1:
            return initExprConstant(0 // 1)
        elif base.value == 1 // 1:
            return initExprConstant(1 // 1)
        if exponent.kind == exprConstant and exponent.value.isInteger:
            return initExprConstant(pow(base.value, exponent.value.toInt))
    if base.kind == exprFactors:
        # rewrite (a*b*c) ^ d as a^d * b^d * c^d
        var factors = newSeq[SymbolicExpression](base.children.len)
        for i in 0 .. base.children.high:
            factors[i] = constructExponent(base.children[i], exponent)
        result = constructFactors(factors)
    elif base.kind == exprExponent:
        # rewrite (a^b)^c to a ^ (b*c)
        result = constructExponent(base.children[0], constructFactors(@[base.children[1], exponent]))
    #elif base.kind == exprFuncCall and base.funcKind == expFunc: similiar to the above
    else:
        result = initExprExponent()
        result.deps = base.deps + exponent.deps
        result.children = @[base, exponent]

#[proc constructExponentFromExpr*(a, b: SymbolicExpression): SymbolicExpression =
    let tree = constructExponent(a.treeRepr, b.treeRepr)
    result.deps = tree.deps
    result.treeRepr = tree
    ]#




#[ Term-rewriting templates
template addManyExpr{`+` * terms}(terms: SymbolicExpression): SymbolicExpression =
    constructTerms(terms)

template mulManyExpr{`*` * factors}(factors: SymbolicExpression): SymbolicExpression =
    constructFactors(factors)
]#

template `-`*(a: SymbolicExpression): SymbolicExpression =
    -1 * a

template `-`*(a, b: SymbolicExpression): SymbolicExpression =
    a + -b

proc `$`*(symExpr: SymbolicExpression): string =
    prettyString(symExpr)

proc `+`*(a, b: SymbolicExpression): SymbolicExpression =
    result = constructTerms(@[a, b])
    #raise newException(ValueError, "`+` proc shouldn't be called. If you see this it means that the term rewriting macro has failed to see this addition. Please report this issue on the Github repo along with you code")

proc `*`*(a, b: SymbolicExpression): SymbolicExpression =
    result = constructFactors(@[a, b])
    #raise newException(ValueError, "`*` proc shouldn't be called. If you see this it means that the term rewriting macro has failed to see this addition. Please report this issue on the Github repo along with you code")

proc `^`*(a, b: SymbolicExpression): SymbolicExpression =
    constructExponent(a, b)

proc `/`*(a, b: SymbolicExpression): SymbolicExpression =
    a * constructExponent(b, someNumberToSymExpr(-1))

template `+=`*(a, b: SymbolicExpression) =
    a = a + b

proc sin*(a: SymbolicExpression): SymbolicExpression =
    if a.kind == exprConstant:
        if a.value == 0 // 1:
            return initExprConstant(0 // 1)
    if a.kind == exprVariable:
        if a.name == pi_str:
            return initExprConstant(0 // 1)
    result = initExprFuncCall(sinFunc)
    result.children = @[a]
    result.deps = a.deps

proc cos*(a: SymbolicExpression): SymbolicExpression =
    if a.kind == exprConstant:
        if a.value == 0 // 1:
            return initExprConstant(1 // 1)
    if a.kind == exprVariable:
        if a.name == pi_str:
            return initExprConstant(-1 // 1)
    result = initExprFuncCall(cosFunc)
    result.children = @[a]
    result.deps = a.deps

proc tan*(a: SymbolicExpression): SymbolicExpression =
    if a.kind == exprConstant:
        if a.value == 0 // 1:
            return initExprConstant(0 // 1)
    if a.kind == exprVariable:
        if a.name == pi_str:
            return initExprConstant(0 // 1)
    result = initExprFuncCall(tanFunc)
    result.children = @[a]
    result.deps = a.deps

proc ln*(a: SymbolicExpression): SymbolicExpression =
    if a.kind == exprConstant:
        if a.value == 0 // 1:
            raise newException(ValueError, "ln(0) is undefined, wait until limits are added...")
        elif a.value == 1 // 1:
            return initExprConstant(0 // 1)
    if a.kind == exprFuncCall and a.funcKind == expFunc:
        return a.children[0]
    result = initExprFuncCall(lnFunc)
    result.children = @[a]
    result.deps = a.deps


proc exp*(a: SymbolicExpression): SymbolicExpression =
    if a.kind == exprConstant:
        if a.value == 0 // 1:
            return initExprConstant(1 // 1)
    if a.kind == exprFuncCall and a.funcKind == lnFunc:
        return a.children[0]
    result = initExprFuncCall(expFunc)
    result.children = @[a]
    result.deps = a.deps

proc diffFuncCall(dVar: SymbolicVariable, tree: SymbolicExpression): SymbolicExpression =
    let child = tree.children[0]
    case tree.funcKind
    of sinFunc:
        result = constructFactors(@[cos(child), diff_internal(dVar, child)])
        return
    of cosFunc:
        result = constructFactors(@[someNumberToSymExpr(-1), sin(child), diff_internal(dVar, child)])
        return
    of tanFunc:
        result = constructFactors(@[constructExponent(cos(child), someNumberToSymExpr(-2)), diff_internal(dVar, child)])
        return
    of expFunc:
        result = constructFactors(@[tree, diff_internal(dVar, child)])
        return
    of lnFunc:
        result = constructFactors(@[diff_internal(dVar, child), constructExponent(child, someNumberToSymExpr(-1))])
        return

proc diff_internal(dVar: SymbolicVariable, tree: SymbolicExpression): SymbolicExpression =
    if not (dVar in tree.deps):
        # This should take care of constants
        return initExprConstant(0 // 1)
    case tree.kind:
    of exprVariable:
        if tree.name == dVar.name:
            return initExprConstant(1 // 1)
        else:
            echo &"A variable has a dependece on another: {tree.name}({dVar.name})"
            return initExprConstant(0 // 1)
    of exprTerms:
        var termDiffs = newSeq[SymbolicExpression](tree.children.len)
        for i in 0 .. tree.children.high:
            termDiffs[i] = diff_internal(dVar, tree.children[i])
        result = constructTerms(termDiffs)
        return
    of exprFactors:
        var factorDiffTerms = newSeq[SymbolicExpression](tree.children.len)
        for i in 0 .. tree.children.high:
            var childCopy = tree.children
            childCopy[i] = diff_internal(dVar, childCopy[i])
            factorDiffTerms[i] = constructFactors(childCopy)
        result = constructTerms(factorDiffTerms)
        return
    of exprExponent:
        let base = tree.children[0]
        let exponent = tree.children[1]
        #result = constructFactors(@[base ^ constructTerms(@[exponent, someNumberToSymTree(-1)]), constructTerms(@[constructFactors(@[exponent, diff_internal(dVar, base)]), constructFactors(@[base, diff_internal(dVar, exponent), ln(base)])])])
        result = constructTerms(@[constructFactors(@[exponent, diff_internal(dVar, base), base ^ constructTerms(@[exponent, -1])]), constructFactors(@[base ^ exponent, ln(base), diff_internal(dVar, exponent)])])
    of exprFuncCall: return diffFuncCall(dVar, tree)
    of exprConstant: echo &"The constant {tree.value} should have been caught in the if-statement above! This is wrong!"

proc diff*(symExpr: SymbolicExpression, dVar: SymbolicVariable, derivOrder = 1): SymbolicExpression =
    if derivOrder == 0: return symExpr
    elif derivOrder == 1: return diff_internal(dVar, symExpr)
    result = diff_internal(dVar, symExpr)
    for i in 2 .. derivOrder:
        result = diff_internal(dVar, result)

proc diff*(symExpr: SymbolicExpression, dVars: varargs[SymbolicVariable]): SymbolicExpression =
    if dVars.len == 0: return symExpr
    elif dVars.len == 1: return diff_internal(dVars[0], symExpr)
    result = diff_internal(dVars[0], symExpr)
    for i in 1 .. dVars.high:
        result = diff_internal(dVars[i], result)



when isMainModule:
    import numericalnim
    var x = newVariable("x")
    var y = newVariable("y")
    var a = x + y
    echo x + 1 - 1 - 2 + 2
    echo x * x * -a
    #timeit(equal(a * 2 + y * (x + y), a * 2 + y * (x + y)), 10000, "With TR")
    echo 3*(x * y + 2 * x * 3)
    echo equal(a * 2 + y + x + y, 2*(y+x) + x + y + y)
    echo a * 2
    echo 2*(y+x)
    let c = 2*x*a + x
    let d = 2 * x * (y + x) + x
    #echo 2*(x+x) - 7 * y # seems like the term rewriting macro can't do enough passes to cover all expressions.
    echo c
    echo d
    echo equal(c, d)
    #timeit(equal(a * 2 + y * (x + y), a * 2 + y * (x + y)), 10000, "Without TR")
    echo equal(x*y, y*x)
    echo equal(x + y, y + x)
    echo 1 * x
    echo someNumberToSymExpr(1) * someNumberToSymExpr(1)
    echo x - x
    echo sym_pi * sym_e
    echo "Kolla här:"
    echo (x + 2 - 1 + 4) * (y + 1 - 1)
    echo x + x + x
    echo 3*x + x + x
    echo (x*y + 1) + (x*y + 1)
    echo 5*(x*y + 1) + (x*y + 1)
    echo 5*(x*y + 1) - 7*(x*y + 1)
    echo x*y + x*y
    echo 2*x*y + x*y + 3 * x*y
    echo 3*x*y - 7 * x*y
    echo 2*x*y + x*y + 3 * x*y + y*x + y*x*x
    echo x / 2
    echo x / y
    echo "Fuse mult:"
    echo x * x
    echo x * x * x ^ 3
    echo x^2 * x^4
    echo x ^ 6 / x ^ 3
    echo (x+y*1) ^ 2 / (x+y) ^ 2
    echo constructTerms(@[someNumberToSymExpr(1), someNumberToSymExpr(-1)])
    echo (x*2 + 4*7) ^ (x*y + y*x)
    echo ((x*2 + 4*7) ^ (x*y + y*x)).deps
    echo "Derivatives:"
    echo "dy/dx: ", diff_internal(x, y)
    echo "d/dx(y + x): ", diff_internal(x, y + x)
    echo "d/dx(2): ", diff_internal(x, someNumberToSymExpr(2))
    echo "d/dx(x*(x+1)): ", diff_internal(x, x*(x+1))
    echo "d/dx(x*(x+1)*y): ", diff_internal(x, x*(x+1)*y)
    timeit(diff_internal(x, x*(x+1)*y*(x+2)*(x+3)), 1000, "Deriv")
    echo diff_internal(x, x*(x+1)*y*(x+2)*(x+3))
    echo sin(x+1) + sin(1+x) + x*y
    echo "sin(sym_pi) = ", sin(sym_pi)
    echo cos(x+2) + cos(0 // 1) + cos(sym_pi + 1)
    echo ln(x) + ln(3 // 1)
    echo exp(0 // 1) + exp(1 // 1) + exp(2*x) + exp(2*x)
    echo ln(exp(x+1))
    echo exp(ln(x+2*y))
    echo tan(x)
    echo tan(0 // 1) + tan(sym_pi)
    echo "d/dx(sin(x)) = ", diff_internal(x, sin(x))
    echo "d/dx(sin(2x)) = ", diff_internal(x, sin(2*x))
    echo "d/dx(cos(x)) = ", diff_internal(x, cos(x))
    echo "d/dx(cos(2x)) = ", diff_internal(x, cos(2*x))
    echo "d/dx(tan(x)) = ", diff_internal(x, tan(x))
    echo "d/dx(tan(2x)) = ", diff_internal(x, tan(2*x))
    echo "d/dx(exp(x)) = ", diff_internal(x, exp(x))
    echo "d/dx(exp(2x)) = ", diff_internal(x, exp(2*x))
    echo "d/dx(ln(x)) = ", diff_internal(x, ln(x))
    echo "d/dx(ln(2x)) = ", diff_internal(x, ln(2*x))
    echo "d/dx(x^2) = ", diff_internal(x, x ^ 2)
    echo "d/dx(x^x) = ", diff_internal(x, x ^ x)
    echo "d/dx(x^x) = ", diff(x ^ x, x)
    echo "d/dx((x+2)^3) = ", diff_internal(x, (x+2)^3)
