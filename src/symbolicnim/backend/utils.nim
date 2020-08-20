import rationals, math

proc rationalToString*(r: Rational[int], withSign = true): string =
  var r = r
  if not withSign and r < 0 // 1:
    r = -r
  if r.den == 1:
    return $r.num
  return $r

proc isInteger*(r: Rational[int]): bool =
  r.den == 1

proc sgn*(r: Rational[int]): Rational[int] =
    let numSgn = sgn(r.num)
    let denSgn = sgn(r.den)
    let totalSgn = numSgn * denSgn
    result = totalSgn.toRational

proc pow*(r: Rational[int], e: int): Rational[int] =
    if e < 0:
        result.num = r.den ^ (-e)
        result.den = r.num ^ (-e)
    else:
        result.num = r.num ^ e
        result.den = r.den ^ e