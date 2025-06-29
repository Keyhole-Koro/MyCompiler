{-# LANGUAGE TupleSections #-}
module CodeGen (codeGen) where

import Parser (AST(..))
import Data.List  (intercalate, (\\))
import qualified Data.Set  as S
import qualified Data.Map  as M

-- ---------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------

type Offset = Int
type Env    = M.Map String Offset
type NameMap = M.Map String String  -- Original name -> mangled name

-- ---------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------

codeGen :: AST -> String
codeGen (Program funs) =
  let nameMap = M.fromList [ (n, if n == "main" then "__START__" else n ++ "_f")
                           | Function n _ _ <- funs ]
  in intercalate "\n\n" (map (codeFun nameMap) funs)
codeGen ast = error $ "codeGen: unsupported top-level AST: " ++ show ast

-- ---------------------------------------------------------------------
-- Function generation
-- ---------------------------------------------------------------------

codeFun :: NameMap -> AST -> String
codeFun nameMap (Function name params body)
  | length params > 4 =
      error $ name_f ++ ": >4 parameters not supported"
  | otherwise =
      unlines $
           [ name_f ++ ":" ]
        ++ prologue
        ++ paramAsm
        ++ concatMap (fst . genStmt nameMap env0) body
        ++ epilogue
  where
    name_f = nameMap M.! name

    prologue =
      [ ""
      , "  ; --- prologue --------------------------------------------------"
      , "  push bp                      ; save previous base pointer"
      , "  mov  bp, sp                  ; establish new base pointer"
      , ""
      ]

    epilogue
      | name_f == "__START__" = []
      | otherwise =
          [ ""
          , "  ; --- epilogue --------------------------------------------------"
          , "  pop  bp                      ; restore previous base pointer"
          , "  mov  pc, lr                  ; return to caller"
          ]


    (paramAsm, paramEnv) = storeParams params
    locals        = collectLocals body \\ params
    startOffset   = - (4 * (length params + 1))
    localEnv      = M.fromList (zip locals [startOffset, startOffset - 4 ..])
    env0          = paramEnv `M.union` localEnv

codeFun _ ast = error $ "codeFun: unsupported function AST"

collectLocals :: [AST] -> [String]
collectLocals = concatMap go
  where
    go (DefVar v _)    = [v]
    go (AssignVar v _) = [v]
    go _               = []

-- ---------------------------------------------------------------------
-- Store parameters r0–r3 → stack frame
-- ---------------------------------------------------------------------

argRegs :: [String]
argRegs = ["r1","r2","r3"]

storeParams :: [String] -> ([String], Env)
storeParams ps =
  ( [ "  mov  r4, bp                  ; copy base pointer to r4", "" ]
    ++ concat asmLines
  , M.fromList envPairs )
  where
    trip = zip3 ps [4,8..] [0..]
    (asmLines, envPairs) = unzip (map mkLine trip)

    mkLine (p, off, idx) =
      ( [ "  ; --- store parameter '" ++ p ++ "' (arg" ++ show idx ++ ") ------------------"
        , ""
        , "  mov   r2, " ++ argRegs !! idx ++ "           ; move argument '" ++ p ++ "'"
        , "  movis   r0, " ++ show off ++ "                               ; load base pointer"
        , "  sub   r4, r0                                ; compute address bp - " ++ show off
        , "  store r4, r2                                ; store '" ++ p ++ "' at [bp - " ++ show off ++ "]"
        , ""
        ]
      , (p, -off) )

-- ---------------------------------------------------------------------
-- Statement generation
-- ---------------------------------------------------------------------

genStmt :: NameMap -> Env -> AST -> ([String], S.Set String)

genStmt nameMap env (DefVar v rhs) =
  let (rhsAsm, rhsVars) = genExpr nameMap env rhs
      off               = env M.! v
  in ( rhsAsm ++
       [ ""
       , "  ; define variable '" ++ v ++ "'"
       , "  mov  r2, bp"
       , "  addis r2, " ++ show off
       , "  store r2, r1"
       ]
     , rhsVars )

genStmt nameMap env (AssignVar v rhs) =
  let (rhsAsm, rhsVars) = genExpr nameMap env rhs
      off               = env M.! v
  in ( rhsAsm ++
       [ ""
       , "  ; " ++ v ++ " = rhs"
       , "  mov  r2, bp"
       , "  addis r2, " ++ show off
       , "  store r2, r1"
       ]
     , rhsVars )

genStmt nameMap env (Return e) =
  let (code, vars) = genExpr nameMap env e
  in ( code ++
       [ ""
       , "  ; --- return ------------------------------------------------------"
       ]
     , vars )

genStmt nameMap env (ExprStmt e) =
  let (code, vars) = genExpr nameMap env e in (code, vars)

genStmt _ _ ast =
  error $ "genStmt: unsupported statement: " ++ show ast

-- ---------------------------------------------------------------------
-- Expression generation
-- ---------------------------------------------------------------------

genExpr :: NameMap -> Env -> AST -> ([String], S.Set String)

genExpr _ _ (IntLit n)
  | n >= 0    = (["  movi  r1, " ++ show n ++ "                ; load constant " ++ show n], S.empty)
  | otherwise = (["  movis r1, " ++ show n ++ "                ; load negative constant"],  S.empty)

genExpr _ env (Var v) =
  case M.lookup v env of
    Just off ->
      ( [ "\n  ; expression: variable '" ++ v ++ "'"
        , "  mov   r1, bp                      ; load base pointer"
        , "  addis  r1, " ++ show off ++ "                      ; offset for '" ++ v ++ "'"
        , "  load  r1, r1                      ; load value of '" ++ v ++ "'"
        ]
      , S.singleton v )
    Nothing ->
      ( [ "  movi  r2, " ++ v ++ "                ; address of global '" ++ v ++ "'"
        , "  load  r1, r2                      ; load global '" ++ v ++ "'"
        ]
      , S.singleton v )

genExpr nameMap env (Add a b) =
  let (aC, aV) = genExpr nameMap env a
      (bC, bV) = genExpr nameMap env b
  in ( aC ++
       [ "  push r1                            ; save left operand"
       , ""
       ] ++
       bC ++
       [ "  pop  r2                            ; restore left operand"
       , "  add  r1, r2                        ; add r1 + r2"
       ]
     , aV `S.union` bV )

genExpr nameMap env (Sub a b) =
  let (aC, aV) = genExpr nameMap env a
      (bC, bV) = genExpr nameMap env b
  in ( aC ++
       [ "  push r1                            ; save left operand"
       , ""
       ] ++
       bC ++
       [ "  pop  r2                            ; restore left operand"
       , "  sub  r1, r2                        ; subtract r2 from r1"
       ]
     , aV `S.union` bV )

genExpr nameMap env (Call fname args)
  | length args > 3 =
      error $ fname ++ ": call with >3 args not supported"
  | otherwise =
      let label = M.findWithDefault fname fname nameMap
          argPieces = map (genExpr nameMap env) args
          argAsm = concat
            [ code ++
              [ "  mov " ++ reg ++ ", r1           ; move argument into " ++ reg
              , ""
              ]
            | (reg, (code, _)) <- zip argRegs argPieces
            ]
          argVars = map snd argPieces
      in ( ["  ; --- call function '" ++ fname ++ "' ----------------------------------"] ++
            argAsm ++
            [ "  jmp " ++ label ++ "                     ; call function '" ++ fname ++ "'" ]
         , S.unions argVars )

genExpr _ _ ast =
  error $ "genExpr: unsupported expression: " ++ show ast
