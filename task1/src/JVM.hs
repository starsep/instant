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
  Load Loc |
  Swap

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
  show Swap = "swap"

type JVMResult = [JVMInstruction]
type Loc = Int
type JVMState = (Map Ident Loc, Loc)
type JVMMonad = RWS () JVMResult JVMState ()

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
  Swap -> 0

maxStack :: [JVMInstruction] -> Int
maxStack ins = maxStack' ins 0

maxStack' :: [JVMInstruction] -> Int -> Int
maxStack' [] acc = acc
maxStack' (i:is) acc = max acc (maxStack' is (acc + stackModifier i))

defaultCtor :: [String]
defaultCtor = [
  ".method public <init>()V",
  "aload_0",
  "invokespecial java/lang/Object/<init>()V",
  "return",
  ".end method"]

classHeader :: String -> [String]
classHeader className = [
  ".class public " ++ className,
  ".super java/lang/Object",
  ""] ++
  defaultCtor

header :: String -> [String]
header className =
  classHeader className ++ [ "",
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
  SExp expr -> do
    tell [GetPrintStream]
    transExp expr
    tell [PrintInt]

transBinExp :: JVMInstruction -> Exp -> Exp -> JVMMonad
transBinExp ins exp1 exp2 = do
  transExp exp1
  transExp exp2
  tell [ins]

transExp :: Exp -> JVMMonad
transExp x = case x of
  ExpAdd exp1 exp2 -> transBinExp Add exp1 exp2
  ExpSub exp1 exp2 -> transBinExp Sub exp1 exp2
  ExpMul exp1 exp2 -> transBinExp Mul exp1 exp2
  ExpDiv exp1 exp2 -> transBinExp Div exp1 exp2
  ExpLit integer -> tell [PushConst integer]
  ExpVar ident -> do
    (state, _) <- get
    tell [Load $ state ! ident]

footer :: [String]
footer = [
  "return",
  ".end method"]

limits :: JVMResult -> Int -> [String]
limits result locals = [
  ".limit stack " ++ show (maxStack result),
  ".limit locals " ++ show locals]

runCompiler :: Program -> (Loc, JVMResult)
runCompiler prog =
  let initState = (Map.empty, 1)
      (_, (_, locals), result) = runRWS (transProgram prog) () initState in
  (locals, result)

isCalculation :: JVMInstruction -> Bool
isCalculation ins = case ins of
  Add -> True
  Sub -> True
  Mul -> True
  Div -> True
  PushConst _ -> True
  Load _ -> True
  _ -> False

extractCalculations' :: JVMResult -> JVMResult -> (JVMResult, JVMResult)
extractCalculations' acc suffix@(ins:rest) =
  if isCalculation ins then
    extractCalculations' (acc ++ [ins]) rest
  else
    (acc, suffix)

extractCalculations :: JVMResult -> (JVMResult, JVMResult)
extractCalculations = extractCalculations' []

isPrint :: JVMInstruction -> Bool
isPrint PrintInt = True
isPrint _ = False

optimizeGetPrintSwap' :: JVMResult -> JVMResult -> JVMResult
optimizeGetPrintSwap' prefix [] = prefix
optimizeGetPrintSwap' prefix rest@(GetPrintStream : ins) =
  let (calc, suffix) = extractCalculations ins
      oldPrefix = prefix ++ [GetPrintStream] in
  if not $ isPrint $ head suffix then
    optimizeGetPrintSwap' oldPrefix ins
  else (
    let oldResult = oldPrefix ++ ins
        newPrefix = prefix ++ calc ++ [GetPrintStream, Swap]
        newResult = newPrefix ++ suffix
        oldStack = maxStack oldResult
        newStack = maxStack newResult in
    if newStack < oldStack then
      optimizeGetPrintSwap' newPrefix suffix
    else
      optimizeGetPrintSwap' oldPrefix ins)
optimizeGetPrintSwap' prefix (i:is) = optimizeGetPrintSwap' (prefix ++ [i]) is

-- optimize by transformation:
-- from: prefix ++ [GetPrintStream] ++ calculations ++ [PrintInt]
-- to: prefix ++ calculations ++ [GetPrintStream, Swap, PrintInt]
optimizeGetPrintSwap :: JVMResult -> JVMResult
optimizeGetPrintSwap = optimizeGetPrintSwap' []

optimize :: JVMResult -> JVMResult
optimize ins =
  optimizeGetPrintSwap ins

compile :: Bool -> String -> Program -> String
compile optimizeOn className prog =
  let (locals, notOptimized) = runCompiler prog
      result = if optimizeOn then optimize notOptimized else notOptimized
      code = foldr ((:) . show) [] result in
  indent (
    header className ++
    limits result locals ++
    code ++
    footer)
