{
module Parser where
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  int       { TokenInt }
  return    { TokenReturn }
  ident     { TokenIdent $$ }
  intlit    { TokenIntLit $$ }
  '='       { TokenAssign }
  '+'       { TokenPlus }
  ';'       { TokenSemi }
  '('       { TokenLParen }
  ')'       { TokenRParen }
  '{'       { TokenLBrace }
  '}'       { TokenRBrace }

%%

Program : Function                    { $1 }

Function : int ident '(' ')' '{' Stmt '}' { Function $2 $6 }

Stmt : return Expr ';'                { Return $2 }

Expr : intlit                         { IntLit $1 }
     | ident                          { Var $1 }
     | Expr '+' Expr                  { Add $1 $3 }

{
data AST
  = Function String AST
  | Return AST
  | IntLit Int
  | Var String
  | Add AST AST
  deriving (Show)

parseError _ = error "Parse error"
}
