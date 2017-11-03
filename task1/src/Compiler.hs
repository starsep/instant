module Main (main) where

import ParInstant
import AbsInstant
import ErrM
import System.Environment (getArgs)
import System.Exit (die)
import Control.Monad
import qualified JVM
import qualified LLVM
import System.IO (openFile, IOMode(ReadMode), hGetContents)

binaryName :: String
binaryName = "./Compiler"

parseArgs :: IO [String]
parseArgs = do
    args <- getArgs
    when (length args /= 2) $
        die $ "Usage: " ++ binaryName ++ " LANG SOURCE"
    return args

checkLang :: String -> IO ()
checkLang lang =
    unless (lang `elem` ["jvm", "llvm"]) $
        die $ "Unknown language: " ++ lang

readSource :: String -> IO String
readSource filename = do
    file <- openFile filename ReadMode
    hGetContents file

compiler :: String -> (Program -> String)
compiler lang =
    if lang == "jvm" then
        JVM.compile
    else
        LLVM.compile

main :: IO ()
main = do
    [lang, filename] <- parseArgs
    checkLang lang
    source <- readSource filename
    program <- case pProgram (myLexer source) of
        Ok p -> return p
        Bad msg -> die "Lexing failed"
    putStrLn $ compiler lang program
