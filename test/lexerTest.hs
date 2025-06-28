module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Parser        (parse)
import CodeGen       (codeGen)
import Lexer         (alexScanTokens)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testCase "Compile sample.my\n" $ do
  src   <- readFile "test/inputs/expr.m"
  let ast  = parse (alexScanTokens src)
      code = codeGen ast

  putStrLn code

  -- Option 2: Use a golden file (comment Option 1 if you use this)
  -- expected <- readFile "test/expected/expr.s"
  -- code @?= expected
