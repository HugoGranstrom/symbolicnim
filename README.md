# SymbolicNim

![test](https://github.com/HugoGranstrom/symbolicnim/workflows/test/badge.svg)

A symbolic library written purely in Nim with the ability to compile expressions into efficient functions.

## Requires Nim 1.2.6 to run!

# Basic usage
SymbolicNim exposes a interface of variables and expressions to the user. Symbolic variables are created with the `newVariable` proc:
```nim
let x = newSymbol("x")
let y = newSymbol("y")
```
The name of the variable is used to identify it and is used in printing. If two variables are given the same name, they will be interpreted as equal. If you want to create a lot of variables easily you can do it with the `createSymbols` macro:
```nim
createSymbols(x, y, coolVar)
```
This is transformed to:
```nim
let x = newSymbol("x")
let y = newSymbol("y")
let coolVar = newSymbol("coolVar")
```
Expressions are created when we do arithmetic with variables and constants:
```nim
let expr1 = 2 * x
let expr2 = x * y + 1 - x ^ 2
let expr3 = sin(x*y) / cos(2*sym_pi) # sym_pi is an exported variable that SymbolicNim interprets as pi.
let expr4 = exp(3*x) * ln(exp(y^5))
# expressions can be used as well:
let expr5 = expr1 - expr2
let expr6 = expr4 / expr3
```

## Generating efficient Nim procs
One of SymbolicNim's main features is that it can do the symbolic calculations at compiletime and generate a fast Nim proc that get's optimized by the compiler. It could be useful if it's used in a tight loop for example. The syntax for generating the procs is to use the `generate` macro:
```nim
let x {.compileTime.} = newSymbol("x") # we must define all the variables we want to use at compileTime with the {.compileTime.} pragma.
let y {.compileTime.} = newSymbol("y")
let z {.compileTime.} = newSymbol("z")
let exprToCompile {.compileTime.} = exp(x*y) ^ sin(z/x) + 1 # this is the expression we want to generate into a function
exprToCompile.generate:
  proc f(x, y, z: float): float
  proc fInline(x, y, z: float): float {.inline.} # you can define how many procs as you want with pragmas and different types

echo f(2.0, 3.0, 4.0)
```
That wasn't that hard, wasn't it? If you are not sure of the types of the arguments you can try `auto` and see if it works. One thing to be aware of is that you must be careful when mixing floats and ints. 

## Derivatives
SymbolicNim can perform symbolic differentitation:
```nim
let x = newSymbol("x")
let y = newSymbol("y")
echo diff(x^2, x) # 2*x
echo diff(sin(2*y), y) # 2*cos(2*y)
echo diff(x^2, x, 2) # d^2/dx^2(x^2) = 2
echo diff(x*y, x, y) # d/dy(d/dx(x*y)) = 1
```
As you can see there are two different ways to call `diff`:
- ONE variable and an int: `diff(x^2, x, 2)`. That is the second derivative with respect to `x`.
- A varargs of variables: `diff(x*y, x, y)`. The derivatives are performed from left to right so the first variable is associated with the innermost derivative. 

Note: SymbolicNim's simplification algorithms aren't anything to brag about really so expect that some expressions can look quite ugly and long even though there is a "easy" simplification that could make it neater.

## Subs
`subs` does what you would expect, it substitutes one expression with another:
```nim
echo subs(sin(x+y), x + y, z) # sin(z)
```

## Taylor expansions
SymbolicNim can calculate Taylor expansions up to 20 terms at the moment. If we want to get the first 10 terms of the taylor series of `sin(x)` around `x = 0` we just have to:
```nim
echo taylor(sin(x), x, 0, 10)
```

## Constants
Symbolic constants are represented as Nim's stdlib `Rational[int]` from [rationals](https://nim-lang.org/docs/rationals.html). Ints and floats are automatically converted to it when entered. Hence if you input a float it will be converted to the best matching fraction and the output may look quite ugly. If you want to write a fraction exactly you can use the `//` proc that will create a Rational:
```nim
let frac = 1 // 3 # exactly 1 / 3 and not 0.3333333333332 or something.
```

# Status: Alpha
This package is in it's very first stages and will have tonnes of bugs everywhere. If you find something not working, I would very much appreciate if you filed a issue :D
