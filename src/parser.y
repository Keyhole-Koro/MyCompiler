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
  ';'       { TokenSemi }
  ','       { TokenComma }
  '('       { TokenLParen }
  ')'       { TokenRParen }
  '{'       { TokenLBrace }
  '}'       { TokenRBrace }

%%                             -- ← don’t remove this separator!

Program   : FunList                       { Program $1 } 

FunList   : Function                      { [$1] }
          | Function FunList              { $1 : $2 }

-- ---------- function definition ----------
Function  : int ident '(' ParamsOpt ')'       -- ★ param list
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
          | Expr  ';'                         { ExprStmt $1 }   -- ★ call as stmt

VarDef    : int ident '=' Expr                { DefVar $2 $4 }

-- ---------- expressions ----------
Expr      : intlit                            { IntLit $1 }
          | ident '(' ArgsOpt ')'             { Call $1 $3 }    -- ★ call w/ args
          | ident                             { Var $1 }
          | Expr '+' Expr                     { Add $1 $3 }

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
  | Return   AST
  | ExprStmt AST
  | IntLit   Int
  | Var      String
  | Add      AST AST
  | Call     String [AST]
  deriving (Show)


parseError _ = error "Parse error"
}

