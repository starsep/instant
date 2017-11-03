module JVM (compile) where

import AbsInstant
import Control.Monad
import Control.Monad.RWS (RWS, get, tell, put, runRWS)
import Data.Map (Map, (!))
import qualified Data.Map as Map

data JVMInstruction =
  GetPrintStream |
  PrintInt |
  PushConst Integer |
  Add |
  Sub |
  Mul |
  Div |
  Store Loc |
  Load Loc

instance Show JVMInstruction where
  show GetPrintStream = "getstatic java/lang/System/out Ljava/io/PrintStream;"
  show PrintInt = "invokevirtual java/io/PrintStream/println(I)V"
  show (PushConst x)
    | x <= 5 = "iconst_" ++ show x
    | x <= 127 = "bipush " ++ show x
    | x <= 32767 = "sipush " ++ show x
    | otherwise = "ldc " ++ show x
  show Add = "iadd"
  show Sub = "isub"
  show Mul = "imul"
  show Div = "idiv"
  show (Store x) =
    if x `elem` [0..3] then
      "istore_" ++ show x
    else
      "istore " ++ show x
  show (Load x) =
    if x `elem` [0..3] then
      "iload_" ++ show x
    else
      "iload " ++ show x

type JVMEnv = ()
type JVMResult = [JVMInstruction]
type Loc = Int
type JVMState = (Map Ident Loc, Loc)
type JVMMonad = RWS JVMEnv JVMResult JVMState ()

initEnv :: JVMEnv
initEnv = ()

initState :: JVMState
initState = (Map.empty, 1)

indentLine :: String -> String
indentLine "" = "\n"
indentLine line@('.':_) = line ++ "\n"
indentLine line = "\t" ++ line ++ "\n"

indent :: [String] -> String
indent = foldr ((++) . indentLine) ""

stackModifier :: JVMInstruction -> Int
stackModifier ins = case ins of
  GetPrintStream -> 1
  PrintInt -> -2
  PushConst _ -> 1
  Add -> -1
  Sub -> -1
  Mul -> -1
  Div -> -1
  Store _ -> -1
  Load _ -> 1

maxStack :: [JVMInstruction] -> Int
maxStack ins = maxStack' ins 0

maxStack' :: [JVMInstruction] -> Int -> Int
maxStack' [] acc = acc
maxStack' (i:is) acc = max acc (maxStack' is (acc + stackModifier i))

defaultCtor :: String
defaultCtor = indent [
  ".method public <init>()V",
  "aload_0",
  "invokespecial java/lang/Object/<init>()V",
  "return",
  ".end method"]

classHeader :: String -> String
classHeader className = indent [
  ".class public " ++ className,
  ".super java/lang/Object",
  ""] ++
  defaultCtor

header :: String -> String
header className =
  classHeader className ++ indent [ "",
  ".method public static main([Ljava/lang/String;)V"]

transProgram :: Program -> JVMMonad
transProgram (Prog stmts) =
  forM_ stmts transStmt

transStmt :: Stmt -> JVMMonad
transStmt x = case x of
  SAss ident expr -> do
    transExp expr
    (state, nextLoc) <- get
    unless (Map.member ident state) $
      put (Map.insert ident nextLoc state, nextLoc + 1)
    (state', _) <- get
    tell [Store $ state' ! ident]
    return ()
  SExp expr -> do
    tell [GetPrintStream]
    transExp expr
    tell [PrintInt]

transExp :: Exp -> JVMMonad
transExp x = case x of
  ExpAdd exp1 exp2 -> do
    transExp exp1
    transExp exp2
    tell [Add]
  ExpSub exp1 exp2 -> do
    transExp exp1
    transExp exp2
    tell [Sub]
  ExpMul exp1 exp2 -> do
    transExp exp1
    transExp exp2
    tell [Mul]
  ExpDiv exp1 exp2 -> do
    transExp exp1
    transExp exp2
    tell [Div]
  ExpLit integer -> tell [PushConst integer]
  ExpVar ident -> do
    (state, _) <- get
    tell [Load $ state ! ident]

footer :: String
footer = indent [
  "return",
  ".end method"]

runCompiler :: Program -> (Loc, JVMResult)
runCompiler prog =
  let (_, (_, locals), result) = runRWS (transProgram prog) initEnv initState in
  (locals, result)

compile :: String -> Program -> String
compile className prog =
  let (locals, result) = runCompiler prog
      code = foldr ((++) . indentLine . show) "" result in
  header className ++
  indentLine (".limit stack " ++ show (maxStack result)) ++
  indentLine (".limit locals " ++ show locals) ++
  code ++
  footer

---------
