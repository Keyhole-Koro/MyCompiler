{-# LANGUAGE TupleSections #-}
module CodeGen (codeGen) where

import Parser (AST(..))
import Data.List  (intercalate)
import qualified Data.Set  as S
import qualified Data.Map  as M

type Offset = Int
type Env    = M.Map String Offset

-- ---------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------

codeGen :: AST -> String
codeGen (Program funs) = intercalate "\n\n" (map codeFun funs)
codeGen ast            = error $ "codeGen: unsupported top-level AST: " ++ show ast

-- ---------------------------------------------------------------------
-- Function generation
-- ---------------------------------------------------------------------

codeFun :: AST -> String
codeFun (Function name params body)
  | length params > 4 =
      error $ name ++ ": >4 parameters not supported"
  | otherwise =
      unlines $
           [ name ++ ":" ]
        ++ prologue
        ++ paramAsm
        ++ concatMap fst stmtBlocks
        ++ epilogue
  where
    prologue =
      [ ""
      , "  ; --- prologue --------------------------------------------------"
      , "  push bp                      ; save previous base pointer"
      , "  mov  bp, sp                  ; establish new base pointer"
      , ""
      ]

    epilogue =
      [ ""
      , "  ; --- epilogue --------------------------------------------------"
      , "  pop  bp                      ; restore previous base pointer"
      , "  ret                          ; return to caller"
      ]

    (paramAsm, env0) = storeParams params
    stmtBlocks       = map (genStmt env0) body

codeFun ast = error $ "codeFun: unsupported function AST"

-- ---------------------------------------------------------------------
-- Store parameters r0–r3 → stack frame
-- ---------------------------------------------------------------------

argRegs :: [String]
argRegs = ["r0","r1","r2","r3"]

storeParams :: [String] -> ([String], Env)
storeParams ps =
  ( [ "  mov  r4, bp                  ; copy base pointer to r4", "" ]
    ++ concat asmLines
  , M.fromList envPairs )
  where
    trip = zip3 ps [4,8..] [0..]
    (asmLines, envPairs) = unzip (map mkLine trip)

    mkLine :: (String, Offset, Int) -> ([String], (String, Offset))
    mkLine (p, off, idx) =
      ( [ "  ; --- store parameter '" ++ p ++ "' (arg" ++ show idx ++ ") ------------------"
        , ""
        , "  mov   r2, " ++ argRegs !! idx ++ "           ; move argument '" ++ p ++ "'"
        , "  subi  r4, " ++ show off ++ "                ; compute address bp - " ++ show off
        , "  store r4, r2                                ; store '" ++ p ++ "' at [bp - " ++ show off ++ "]"
        , ""
        ]
      , (p, -off) )

-- ---------------------------------------------------------------------
-- Statement generation
-- ---------------------------------------------------------------------

genStmt :: Env -> AST -> ([String], S.Set String)

genStmt env (DefVar v rhs) =
  let (rhsAsm, rhsVars) = genExpr env rhs
  in ( rhsAsm ++
       [ ""
       , "  ; --- define variable '" ++ v ++ "' ------------------------------------------"
       , "  movi r2, " ++ v ++ "                ; address of '" ++ v ++ "'"
       , "  store r2, r1                        ; store value of '" ++ v ++ "'"
       ]
     , S.insert v rhsVars )

genStmt env (Return e) =
  let (code, vars) = genExpr env e
  in ( code ++
       [ ""
       , "  ; --- return ------------------------------------------------------"
       , "  pop  bp                            ; restore base pointer"
       , "  ret                                ; return"
       ]
     , vars )

genStmt env (ExprStmt e) =
  let (code, vars) = genExpr env e in (code, vars)

genStmt _ ast =
  error $ "genStmt: unsupported statement: " ++ show ast

-- ---------------------------------------------------------------------
-- Expression generation
-- ---------------------------------------------------------------------

genExpr :: Env -> AST -> ([String], S.Set String)

genExpr _ (IntLit n)
  | n >= 0    = (["  movi  r1, " ++ show n ++ "                ; load constant " ++ show n], S.empty)
  | otherwise = (["  movis r1, " ++ show n ++ "                ; load negative constant"],  S.empty)

genExpr env (Var v) =
  case M.lookup v env of
    Just off ->
      ( [ "  mov   r1, bp                      ; load base pointer"
        , "  addi  r1, " ++ show off ++ "               ; offset for '" ++ v ++ "'"
        , "  load  r1, r1                      ; load value of '" ++ v ++ "'"
        ]
      , S.singleton v )
    Nothing ->
      ( [ "  movi  r2, " ++ v ++ "                ; address of global '" ++ v ++ "'"
        , "  load  r1, r2                      ; load global '" ++ v ++ "'"
        ]
      , S.singleton v )

genExpr env (Add a b) =
  let (aC, aV) = genExpr env a
      (bC, bV) = genExpr env b
  in ( aC ++
       [ "  push r1                            ; save left operand"
       , ""
       ] ++
       bC ++
       [ "  pop  r2                            ; restore left operand"
       , "  add  r1, r2                        ; add r1 + r2"
       ]
     , aV `S.union` bV )

genExpr env (Call fname args)
  | length args > 4 =
      error $ fname ++ ": call with >4 args not supported"
  | otherwise =
      ( argAsm ++
        [ "  call " ++ fname ++ "                     ; call function '" ++ fname ++ "'" ]
      , S.unions argVars )
  where
    argPieces = map (genExpr env) args

    argAsm =
      concat
        [ code ++
          [ "  mov " ++ reg ++ ", r1           ; move argument into " ++ reg
          , ""
          ]
        | (reg, (code, _)) <- zip argRegs argPieces
        ]

    argVars = map snd argPieces

genExpr _ ast =
  error $ "genExpr: unsupported expression: " ++ show ast
