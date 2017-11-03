module LLVM (compile) where

import AbsInstant
import Data.List (isPrefixOf)
import Control.Monad
import Control.Monad.RWS (RWS, get, tell, put, runRWS)
import Data.Map (Map, (!))
import qualified Data.Map as Map

type Loc = Int
type LLVMResult = [LLVMInstruction]
type LLVMState = (Map Ident Loc, Loc)
type LLVMMonad = RWS () LLVMResult LLVMState

data LLVMInstruction =
  PrintInt Loc |
  Add Loc Loc Loc |
  Sub Loc Loc Loc |
  Mul Loc Loc Loc |
  Div Loc Loc Loc |
  Store Loc Integer

showTwoArgs :: String -> Loc -> Loc-> Loc -> String
showTwoArgs op r x y =
  "%_" ++ show r ++ " = " ++ op ++ " i32 %_" ++ show x ++ ", %_" ++ show y

instance Show LLVMInstruction where
  show (PrintInt x) = "call void @printInt(i32 %_" ++ show x ++ ")"
  show (Add r x y) = showTwoArgs "add" r x y
  show (Sub r x y) = showTwoArgs "sub" r x y
  show (Mul r x y) = showTwoArgs "mul" r x y
  show (Div r x y) = showTwoArgs "sdiv" r x y
  show (Store r x) = "%_" ++ show r ++ " = add i32 0, " ++ show x

indentLine :: String -> String
indentLine line =
  if null line ||
     any (`isPrefixOf` line) ["@", "}", "declare", "define"] then
    line ++ "\n"
  else
    "\t" ++ line ++ "\n"

indent :: [String] -> String
indent = foldr ((++) . indentLine) ""

-- printInt from runtime.ll
header :: String
header = indent [
  "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"",
  "",
  "declare i32 @printf(i8*, ...)",
  "",
  "define void @printInt(i32 %x) {",
  "%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0",
  "call i32 (i8*, ...) @printf(i8* %t0, i32 %x)",
  "ret void",
  "}",
  "",
  "define i32 @main(i32 %argc, i8** %argv) {"]

footer :: String
footer = indent [
  "ret i32 0",
  "}"]

transProgram :: Program -> LLVMMonad ()
transProgram (Prog stmts) =
  forM_ stmts transStmt

transStmt :: Stmt -> LLVMMonad ()
transStmt x = case x of
  SAss ident expr -> do
    v <- transExp expr
    (state, loc) <- get
    put (Map.insert ident v state, loc)
  SExp expr -> do
    r <- transExp expr
    tell [PrintInt r]

newLoc :: LLVMMonad Loc
newLoc = do
  (q, result) <- get
  put (q, result + 1)
  return result

transExpHelper :: Exp -> Exp -> LLVMMonad (Loc, Loc, Loc)
transExpHelper exp1 exp2 = do
  x <- transExp exp1
  y <- transExp exp2
  r <- newLoc
  return (r, x, y)

transExp :: Exp -> LLVMMonad Loc
transExp e = case e of
  ExpAdd exp1 exp2 -> do
    (r, x, y) <- transExpHelper exp1 exp2
    tell [Add r x y]
    return r
  ExpSub exp1 exp2 -> do
    (r, x, y) <- transExpHelper exp1 exp2
    tell [Sub r x y]
    return r
  ExpMul exp1 exp2 -> do
    (r, x, y) <- transExpHelper exp1 exp2
    tell [Mul r x y]
    return r
  ExpDiv exp1 exp2 -> do
    (r, x, y) <- transExpHelper exp1 exp2
    tell [Div r x y]
    return r
  ExpLit integer -> do
    r <- newLoc
    tell [Store r integer]
    return r
  ExpVar ident -> do
    (state, _) <- get
    return $ state ! ident

runCompiler :: Program -> LLVMResult
runCompiler prog =
  let initState = (Map.empty, 0)
      (_, _, result) = runRWS (transProgram prog) () initState in
  result

compile :: Program -> String
compile program =
  let result = runCompiler program
      code = foldr ((++) . indentLine . show) "" result in
  header ++
  code ++
  footer
