import rationals, math

proc rationalToString*(r: Rational[int]): string =
  if r.den == 1:
    return $r.num
  return $r

proc isInteger*(r: Rational[int]): bool =
  r.den == 1

proc pow*(r: Rational[int], e: int): Rational[int] =
    if e < 0:
        result.num = r.den ^ (-e)
        result.den = r.num ^ (-e)
    else:
        result.num = r.num ^ e
        result.den = r.den ^ e