module JVM (compile) where

import AbsInstant
import ErrM
type Result = Err String

indentLine :: String -> String
indentLine line =
    if not (null line) && head line /= '.' then
        "\t" ++ line ++ "\n"
    else
        line ++ "\n"

indent :: [String] -> String
indent = foldr ((++) . indentLine) ""

defaultCtor :: String
defaultCtor = indent [
    ".method public <init>()V",
    "aload_0",
    "invokespecial java/lang/Object/<init>()V",
    "return",
    ".end method"]

classHeader :: String
classHeader =
    ".class public test04\n" ++
    ".super java/lang/Object\n\n" ++
    defaultCtor

header :: String
header =
    classHeader ++
    ".method public static main([Ljava/lang/String;)V\n" ++
    "\treturn\n" ++
    ".end method\n"

compile :: Program -> String
compile (Prog stmts) = header -- ++ show stmts ++ ".jvm"

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Prog stmts -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  SAss ident exp -> failure x
  SExp exp -> failure x
transExp :: Exp -> Result
transExp x = case x of
  ExpAdd exp1 exp2 -> failure x
  ExpSub exp1 exp2 -> failure x
  ExpMul exp1 exp2 -> failure x
  ExpDiv exp1 exp2 -> failure x
  ExpLit integer -> failure x
  ExpVar ident -> failure x
