module Main where

import Lexer
import Parser
import System.Environment

import CodeGen

main :: IO ()
main = do
  [file] <- getArgs
  src <- readFile file
  let tokens = lexer src
  let ast = parse tokens
  let asm = codegen ast
  writeFile "out.asm" (unlines asm)
  putStrLn "Assembly written to out.asm"
