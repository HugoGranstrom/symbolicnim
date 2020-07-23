import symnim

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
echo x ** y
echo (x+1) ** 2
echo (x+1) ** 1
echo (x+1) ** 0
echo 0 ** 0
echo x / 2
echo x / y
echo (x ** 2) ** y
echo ((x+y*2) ** (x+1)) ** ((y+1) ** 2)
echo (x*y) ** -2
echo ((2*x)**(x+1)) ** (3 // 2)
echo (2*x) ** 1
echo "Fuse mult:"
echo x * x
echo x * x * x ^ 3
echo x^2 * x^4
echo x ^ 6 / x ^ 3
echo (x+y*1) ^ 2 / (x+y) ^ 2
echo constructTermsFromExpr(someNumberToSymExpr(1), someNumberToSymExpr(-1))
echo (x*2 + 4*7) ^ (x*y + y*x)
echo ((x*2 + 4*7) ^ (x*y + y*x)).deps
echo "Derivatives:"
echo "dy/dx: ", diff(x, y)
echo "d/dx(y + x): ", diff(x, y + x)
echo "d/dx(2): ", diff(x, someNumberToSymExpr(2))
echo "d/dx(x*(x+1)): ", diff(x, x*(x+1))
echo "d/dx(x*(x+1)*y): ", diff(x, x*(x+1)*y)
#timeit(diff(x, x*(x+1)*y*(x+2)*(x+3)), 1000, "Deriv")
echo diff(x, x*(x+1)*y*(x+2)*(x+3))
echo sin(x+1) + sin(1+x) + x*y
echo "sin(sym_pi) = ", sin(sym_pi)
echo cos(x+2) + cos(0 // 1) + cos(sym_pi + 1)
echo ln(x) + ln(3 // 1)
echo exp(0 // 1) + exp(1 // 1) + exp(2*x) + exp(2*x)
echo ln(exp(x+1))
echo exp(ln(x+2*y))
echo tan(x)
echo tan(0 // 1) + tan(sym_pi)
echo "d/dx(sin(x)) = ", diff(x, sin(x))
echo "d/dx(sin(2x)) = ", diff(x, sin(2*x))
echo "d/dx(cos(x)) = ", diff(x, cos(x))
echo "d/dx(cos(2x)) = ", diff(x, cos(2*x))
echo "d/dx(tan(x)) = ", diff(x, tan(x))
echo "d/dx(tan(2x)) = ", diff(x, tan(2*x))
echo "d/dx(exp(x)) = ", diff(x, exp(x))
echo "d/dx(exp(2x)) = ", diff(x, exp(2*x))
echo "d/dx(ln(x)) = ", diff(x, ln(x))
echo "d/dx(ln(2x)) = ", diff(x, ln(2*x))
echo "d/dx(x^2) = ", diff(x, x ^ 2)
echo "d/dx(x^x) = ", diff(x, x ^ x)
echo "d/dx((x+2)^3) = ", diff(x, (x+2)^3)
echo (x*2).deps
