- Make an AST that works both at Runtime and Compile-time.
- You can switch out all symbolic variables with concrete types using a `generate` macro. Ex:
```
proc sqrtDiff(f, x: SymExpr): SymExpr =
    sqrt(diff(f, x))

sqrtDiff(x^2, x).generate:
    proc sqrtDiff(x: float): float
```
- Does it make sense to generate from a proc in this case? Because we can't perform any operation like diff on a concrete type. We can't differentiate for example if we get the proc `proc (x: float): float = x^2`. That would only be if we parsed the proc and extracted whatever it returned as the expression to generate. But we still wouldn't know what diff(f, x) is without knowing what f is so it must be explicit.

- Conditional branches? If a < pi: 0, else: a^2. Do we have to? Yes we can't just copy syntax of the proc because we need to parse it into an AST.

## Problem: Compiling user-defined types
How can we compile say a custom matrix type? Even if we have a symArray deep embedding we can't know what the interface to it will be. `@[x, 2*y, z^3]` will be compiled but we have no idea of how it should be used. Like a vector? Like a matrix? like a tensor? We must specify a compile proc or something for the type. 
```nim
type
    SymMatrix* = ref object
        data: seq[SymExpr]
        nrows, ncols: int

let A = toMatrix(@[@[x, y^2, 2*z], @[...], @[...]])
let B = toMatrix(@[@[2*t, ln(x), sin(y)], @[...], @[...]])
let C = A * B
generate C: # generate should work on types that supports compilation. Use concepts?   
    proc fC(x, y, z, t: float): ConcreteMatrix[float]
```
```nim
macro compile(matrix: SymMatrix): untyped =
    # 1. turn the underlying seq to its NimNode repr. (could probably be done with a proc from symbolicnim)
    # 2. construct an Arraymancer Tensor from that. ie. construct the code `result = @[].toTensor; result.reshape(nrows, ncols)`. (quote do is your friend)

macro generate(symExpr: typed, signatures: untyped): untyped =
    # 1. check that we get a signature without a body
    # 2. add body from compile:
    # proc signature =
    #   compile(`symExpr`)
```
- The user should be able to reuse the compilation of SymExpr in its own. So it can for example iterate over a seq and construct the final concrete seq in a loop.
- Use the trick of generating the code `compile(matrix/symExpr etc)` and then write macros with typed input argument like this: `macro compile*(matrix: static SymMatrix): untyped`. This way we can access the AST (`static` fixes so we don't only get the identifier) and do the neccecary transform from SymNode -> NimNode. 
- ~~We could perhaps do this automatically by going to definition of whatever type is being generated and look for SymExpr and compile it. And then create a new type with some suffix (make it idiomatic the SymFoo becomes RealFoo or something) that is the final product of the generated proc.~~ No the user must know what type it is being transformed into. It must be done explicitly!
- How to handle a mix of ints and floats? Say we have `x*y` and `x: float, y: int`. 
    1. ~~Check if any argument is float, if so convert all ints to float in proc body.~~ Operations between ints would become less accurate.
    2. Use converter `int -> float`. also a `Rational -> float` converter could be helpful
    3. The user can use `any` if they are unsure of the type that will be outputted.




## Minor ideas:
- Use `Table[kind]` or `array[kind]` for as much as possible. Should hopefully remove a lot of ugly case of statements.
- Use Table[SymExpr, Rational] for add and mul. Cache the hash of expr so that hash() check if cached before calculating again. This could simplify many of the "simplifications" like constant-folding.
- - Addition is represented as Table{symbolicTerm: coefficent} + constant
- - Multiplication is represented as Table{symbolicBase: power} + coefficient
- Function calls should be as generic as possible. A name field and a length field. This way FuncCall(name: "bessel", nargs: 2) can be created and then checking can be done against the name instead of a funcKind. They define `proc bessel(j, n): SymNode` by themselfs. 
- Simplifications should be defined as procs with a pragma (macro) that adds the proc to the global list of simplification procs. Keep one runtime and one compiletime list so we can access it in both cases. This is needed for simplifications like sin^2 + cos^2 = 1. **When should this be called? When calling simplify() and when generating code.** It is only executed for SymNodes though, so it must be handled in the `compile` macro/proc for SymNode. 