# SymbolicNim
A symbolic library written purely in Nim

## Requires Nim devel to run!

# Basic usage
SymbolicNim exposes a interface of variables and expressions to the user. Symbolic variables are created with the `newVariable` proc:
```nim
let x = newVariable("x")
let y = newVariable("y")
```
The name of the variable is used to identify it and is used in printing. If two variables are given the same name, they will be interpreted as equal.
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

## Constants
Symbolic constants are represented as Nim's stdlib `Rational[int]` from [rationals](https://nim-lang.org/docs/rationals.html). Ints and floats are automatically converted to it when entered. Hence if you input a float it will be converted to the best matching fraction and the output may look quite ugly. If you want to write a fraction exaactly you can use the `//` proc that will create a Rational:
```nim
let frac = 1 // 3 # exactly 1 / 3 and not 0.3333333333332 or something.
```

# Status: Alpha
This package is in it's very first stages and will have tonnes of bugs everywhere. If you find something not working, I would very much appreciate if you filed a issue :D
