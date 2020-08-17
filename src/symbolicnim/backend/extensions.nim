import macros, tables
import ./ast_types

export tables

var simplifyProcsCT* {.compileTime.} = newSeq[proc(node: SymNode): SymNode]()
var simplifyProcsRT* = newSeq[proc(node: SymNode): SymNode]()

var compileProcsCT* {.compileTime.} = initTable[string, proc(node: SymNode): NimNode]()#{"": proc(node: SymNode): NimNode = discard}.toTable

var diffProcsCT* {.compileTime.} = initTable[string, proc(node, dVar: SymNode): SymNode]()
var diffProcsRT* = initTable[string, proc(node, dVar: SymNode): SymNode]()


proc isValidFunc*(name: string): bool =
  when nimvm:
    result = name in compileProcsCT and name in diffProcsCT
  else:
    result = name in diffProcsRT



macro simplifyPass*(simpProc: untyped): untyped =
  simpProc.expectKind(nnkProcDef)
  let procName = name(simpProc)
  result = quote do: # just define the proc as usual and add it to simplifyProcs
    `simpProc`
    static: simplifyProcsCT.add `procName`
    simplifyProcsRT.add `procName`

template registerSymFunc*(name: string, diffProc: proc(node, dVar: SymNode): SymNode, compileProc: proc(node: SymNode): NimNode): untyped =
  static:
    assert not isValidFunc(name), "Func name '" & name & "' is already used!"
    compileProcsCT[name] = compileProc
    diffProcsCT[name] = diffProc
  diffProcsRT[name] = diffProc
