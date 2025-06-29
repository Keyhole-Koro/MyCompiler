{
module Parser where
import Lexer
}

%name   parse
%tokentype { Token }
%error  { parseError }

%token
  int       { TokenInt }
  return    { TokenReturn }
  ident     { TokenIdent $$ }
  intlit    { TokenIntLit $$ }
  '='       { TokenAssign }
  '+'       { TokenPlus }
  '-'       { TokenHyphen }
  ';'       { TokenSemi }
  ','       { TokenComma }
  '('       { TokenLParen }
  ')'       { TokenRParen }
  '{'       { TokenLBrace }
  '}'       { TokenRBrace }

%%                             -- ← don’t remove this separator!

Program   : FunList              { Program $1 }

FunList   : Function                      { [$1] }
          | Function FunList              { $1 : $2 }

-- ---------- function definition ----------
Function  : int ident '(' ParamsOpt ')'
            '{' Stmts '}'                     { Function $2 $4 $7 }

ParamsOpt :                                    { [] }
          | ParamList                         { $1 }

ParamList : int ident                         { [$2] }
          | int ident ',' ParamList           { $2 : $4 }

-- ---------- statements ----------
Stmts     :                                   { [] }
          | Stmt Stmts                        { $1 : $2 }

Stmt      : return Expr ';'                   { Return $2 }
          | VarDef ';'                        { $1 }
          | VarAssign ';'                     { $1 }
          | Expr  ';'                         { ExprStmt $1 }

VarDef    : int ident '=' Expr                { DefVar $2 $4 }
VarAssign : ident '=' Expr                    { AssignVar $1 $3 }

-- ---------- expressions ----------
Expr      : intlit                            { IntLit $1 }
          | ident '(' ArgsOpt ')'             { Call $1 $3 }
          | ident                             { Var $1 }
          | Expr '+' Expr                     { Add $1 $3 }
          | Expr '-' Expr                     { Sub $1 $3 }

ArgsOpt   :                                   { [] }
          | ArgList                           { $1 }

ArgList   : Expr                              { [$1] }
          | Expr ',' ArgList                  { $1 : $3 }

{
-- Haskell code AFTER the second %%

data AST
  = Program [AST]
  | Function String [String] [AST]
  | DefVar   String AST
  | AssignVar String AST
  | Return   AST
  | ExprStmt AST
  | Assign   String AST
  | IntLit   Int
  | Var      String
  | Add      AST AST
  | Sub      AST AST
  | Call     String [AST]
  deriving (Show)


parseError _ = error "Parse error"
}

