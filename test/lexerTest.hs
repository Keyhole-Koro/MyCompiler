import Test.Tasty
import Test.Tasty.HUnit
import Lexer -- your lexer module

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Lexer Tests"
  [ testCase "Recognizes keyword 'int'" $
      lexer "int" @?= [TokenInt]

  , testCase "Recognizes keyword 'return'" $
      lexer "return" @?= [TokenReturn]

  , testCase "Recognizes identifier" $
      lexer "hello_world" @?= [TokenIdent "hello_world"]

  , testCase "Recognizes integer literal" $
      lexer "12345" @?= [TokenIntLit 12345]

  , testCase "Recognizes assignment" $
      lexer "=" @?= [TokenAssign]

  , testCase "Recognizes semicolon" $
      lexer ";" @?= [TokenSemi]

  , testCase "Recognizes plus" $
      lexer "+" @?= [TokenPlus]

  , testCase "Recognizes parentheses" $
      lexer "( )" @?= [TokenLParen, TokenRParen]

  , testCase "Recognizes braces" $
      lexer "{ }" @?= [TokenLBrace, TokenRBrace]

  , testCase "Ignores whitespace" $
      lexer "   \n\t  " @?= []

  , testCase "Parses simple declaration" $
      lexer "int x;" @?= [TokenInt, TokenIdent "x", TokenSemi]

  , testCase "Parses assignment statement" $
      lexer "x = 42;" @?= [TokenIdent "x", TokenAssign, TokenIntLit 42, TokenSemi]

  , testCase "Parses return statement" $
      lexer "return x + 1;" @?=
        [TokenReturn, TokenIdent "x", TokenPlus, TokenIntLit 1, TokenSemi]

  , testCase "Parses function call style" $
      lexer "foo(123)" @?=
        [TokenIdent "foo", TokenLParen, TokenIntLit 123, TokenRParen]

  , testCase "Parses block" $
      lexer "{ return x; }" @?=
        [TokenLBrace, TokenReturn, TokenIdent "x", TokenSemi, TokenRBrace]

  , testCase "Handles multiple identifiers" $
      lexer "foo bar123 _baz" @?=
        [TokenIdent "foo", TokenIdent "bar123", TokenIdent "_baz"]
  ]
