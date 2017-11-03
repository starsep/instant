module Main (main) where

import ParInstant
import AbsInstant
import ErrM
import System.Environment (getArgs)
import System.FilePath.Posix (takeBaseName)
import Control.Monad
import qualified JVM
import qualified LLVM
import System.Exit (exitFailure)
import System.IO (openFile, IOMode(ReadMode), hGetContents, hPutStrLn, stderr)

-- import System.Exit (die)
-- doesn't work on ghc 7.6.3
die :: String -> IO ()
die err = hPutStrLn stderr err >> exitFailure

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

compiler :: String -> String -> (Program -> String)
compiler lang basename =
    if lang == "jvm" then
        JVM.compile basename
    else
        LLVM.compile

main :: IO ()
main = do
    [lang, filename] <- parseArgs
    checkLang lang
    let basename = takeBaseName filename
    source <- readSource filename
    program <- case pProgram (myLexer source) of
        Ok p -> return p
        Bad msg -> do
          die $ "Lexing failed: " ++ show msg
          return $ Prog []
    putStrLn $ compiler lang basename program
