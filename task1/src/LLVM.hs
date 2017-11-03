module LLVM (compile) where

import AbsInstant

compile :: Program -> String
compile program = show program ++ ".llvm"
