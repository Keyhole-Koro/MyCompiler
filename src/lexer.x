{
module Lexer where
}

%wrapper "basic"

$white = [\ \t\n\r]
$digit = 0-9
$alpha = [A-Za-z_]
$alphanum = [A-Za-z0-9_]

tokens :-

$white+                      ;
"int"                        { \_ -> TokenInt }
"return"                     { \_ -> TokenReturn }
$alpha $alphanum*            { \s -> TokenIdent s }
";"                          { \_ -> TokenSemi }
"="                          { \_ -> TokenAssign }
"+"                          { \_ -> TokenPlus }
"("                          { \_ -> TokenLParen }
")"                          { \_ -> TokenRParen }
"{"                          { \_ -> TokenLBrace }
"}"                          { \_ -> TokenRBrace }
$digit+                      { \s -> TokenIntLit (read s) }

{
data Token
  = TokenInt
  | TokenReturn
  | TokenIdent String
  | TokenIntLit Int
  | TokenAssign
  | TokenPlus
  | TokenSemi
  | TokenLParen
  | TokenRParen
  | TokenLBrace
  | TokenRBrace
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer = alexScanTokens
}
