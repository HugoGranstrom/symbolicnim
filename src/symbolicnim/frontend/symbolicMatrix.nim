import macros
import ../backend
import ./symbolicExpression
import arraymancer
export arraymancer

type SymMatrix* = ref object
  data*: seq[SymExpr]
  rows*, cols*: Natural

proc `$`*(matrix: SymMatrix): string =
  # use nim-terminaltables?
  "SymMatrix(" & $matrix.rows & "x" & $matrix.cols & "):\n" & $matrix.data

proc newSymMatrix*(rows, cols: Natural): SymMatrix =
  assert rows != 0 and cols != 0, "rows and cols must be greater than 0"
  let N = rows * cols
  new result
  result.data = newSeq[SymExpr](N)
  result.rows = rows
  result.cols = cols

proc `[]`*(matrix: SymMatrix, i, j: Natural): SymExpr =
  when defined(debug): assert matrix.rows * matrix.cols == matrix.data.len, "The dimensions of matrix data and cols*rows doesn't add up! If you haven't done anything spooky with them please open an issue on Github with your code example. :D"
  matrix.data[i*matrix.cols + j]

proc `[]=`*(matrix: var SymMatrix, i, j: Natural, input: SymExpr) =
  when defined(debug): assert matrix.rows * matrix.cols == matrix.data.len, "The dimensions of matrix data and cols*rows doesn't add up! If you haven't done anything spooky with them please open an issue on Github with your code example. :D"
  matrix.data[i*matrix.cols + j] = input

proc toSymMatrix*(s: seq[seq[SymExpr]], rowMajor = true): SymMatrix =
  assert s.len > 0 and s[0].len > 0, "Seqs can't be empty to create a SymMatrix"
  var nrows, ncols: Natural
  if rowMajor:
    nrows = s[0].len
    ncols = s.len
  else:
    ncols = s[0].len
    nrows = s.len
  result = newSymMatrix(nrows, ncols)
  if rowMajor:
    for i in 0 ..< nrows:
      for j in 0 ..< ncols:
        result[i, j] = s[i][j]
  else:
    for i in 0 ..< nrows:
      for j in 0 ..< ncols:
        result[i, j] = s[j][i]

proc toRow*(s: seq[SymExpr]): SymMatrix =
  let nrows = s.len
  assert nrows > 0, "Seq can't be empty to create a SymMatrix"
  new result
  result.data = s
  result.rows = nrows
  result.cols = 1

proc toCol*(s: seq[SymExpr]): SymMatrix =
  let ncols = s.len
  assert ncols > 0, "Seq can't be empty to create a SymMatrix"
  new result
  result.data = s
  result.rows = 1
  result.cols = ncols



proc `+`*(a, b: SymMatrix): SymMatrix =
  assert a.rows == b.rows and a.cols == b.cols, "Dimensions doesn't match!"
  result = newSymMatrix(a.rows, a.cols)
  for i in 0 .. a.data.high:
    result.data[i] = a.data[i] + b.data[i]

proc `-`*(a, b: SymMatrix): SymMatrix =
  assert a.rows == b.rows and a.cols == b.cols, "Dimensions doesn't match!"
  result = newSymMatrix(a.rows, a.cols)
  for i in 0 .. a.data.high:
    result.data[i] = a.data[i] - b.data[i]

proc `*.`*(a, b: SymMatrix): SymMatrix =
  assert a.rows == b.rows and a.cols == b.cols, "Dimensions doesn't match!"
  result = newSymMatrix(a.rows, a.cols)
  for i in 0 .. a.data.high:
    result.data[i] = a.data[i] * b.data[i]

proc `/.`*(a, b: SymMatrix): SymMatrix =
  assert a.rows == b.rows and a.cols == b.cols, "Dimensions doesn't match!"
  result = newSymMatrix(a.rows, a.cols)
  for i in 0 .. a.data.high:
    result.data[i] = a.data[i] / b.data[i]

proc `^.`*(a, b: SymMatrix): SymMatrix =
  assert a.rows == b.rows and a.cols == b.cols, "Dimensions doesn't match!"
  result = newSymMatrix(a.rows, a.cols)
  for i in 0 .. a.data.high:
    result.data[i] = a.data[i] ^ b.data[i]

proc `-`*(a: SymMatrix): SymMatrix =
  result = newSymMatrix(a.rows, a.cols)
  for i in 0 .. a.data.high:
    result.data[i] = -a.data[i]

proc `*`*(a, b: SymMatrix): SymMatrix =
  assert a.cols == b.rows, "Dimensions doesn't match for matrix multiplication"
  result = newSymMatrix(a.rows, b.cols)
  result.rows = a.rows
  result.cols = b.cols
  let equalSide = a.cols
  for i in 0 ..< result.rows:
    for j in 0 ..< result.cols:
      var temp: SymExpr = 0.intToSymExpr
      for k in 0 ..< equalSide:
        temp = temp + a[i, k] * b[k, j]
      result[i, j] = temp

proc transpose*(matrix: SymMatrix): SymMatrix =
  result = newSymMatrix(matrix.cols, matrix.rows)
  for i in 0 .. result.rows:
    for j in 0 .. result.cols:
      result[i, j] = matrix[j, i]

proc t*(matrix: SymMatrix): SymMatrix = transpose(matrix)


macro compile*(matrix: static SymMatrix): untyped =
  discard # construct result = @[matrix.data].toTensor; result = reshape(result, matrix.rows, matrix.cols)
  var elems = nnkBracket.newTree()
  for i in 0 .. matrix.data.high:
    elems.add compileSymExpr(matrix.data[i])
  # constructs the @[...] expression
  let seqExpression = nnkPrefix.newTree(
      newIdentNode("@"),
      elems
    )
  let rows = newLit matrix.rows
  let cols = newLit matrix.cols
  result = quote do:
    result = `seqExpression`.toTensor
    result = result.reshape(`rows`, `cols`)
  echo result.repr


