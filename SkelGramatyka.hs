-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module SkelGramatyka where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsGramatyka

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: AbsGramatyka.Ident -> Result
transIdent x = case x of
  AbsGramatyka.Ident string -> failure x

transProgram :: Show a => AbsGramatyka.Program' a -> Result
transProgram x = case x of
  AbsGramatyka.Program _ stmts -> failure x

transStmt :: Show a => AbsGramatyka.Stmt' a -> Result
transStmt x = case x of
  AbsGramatyka.Decl _ type_ items -> failure x
  AbsGramatyka.FnDecl _ type_ ident args fblock -> failure x
  AbsGramatyka.Empty _ -> failure x
  AbsGramatyka.BStmt _ block -> failure x
  AbsGramatyka.Ass _ ident expr -> failure x
  AbsGramatyka.Incr _ ident -> failure x
  AbsGramatyka.Decr _ ident -> failure x
  AbsGramatyka.Cond _ expr stmt -> failure x
  AbsGramatyka.CondElse _ expr stmt1 stmt2 -> failure x
  AbsGramatyka.While _ expr stmt -> failure x
  AbsGramatyka.SExp _ expr -> failure x

transArg :: Show a => AbsGramatyka.Arg' a -> Result
transArg x = case x of
  AbsGramatyka.VArg _ type_ ident -> failure x
  AbsGramatyka.RArg _ type_ ident -> failure x

transRet :: Show a => AbsGramatyka.Ret' a -> Result
transRet x = case x of
  AbsGramatyka.Ret _ expr -> failure x

transFBlock :: Show a => AbsGramatyka.FBlock' a -> Result
transFBlock x = case x of
  AbsGramatyka.FBlock _ stmts ret -> failure x

transBlock :: Show a => AbsGramatyka.Block' a -> Result
transBlock x = case x of
  AbsGramatyka.Block _ stmts -> failure x

transItem :: Show a => AbsGramatyka.Item' a -> Result
transItem x = case x of
  AbsGramatyka.NoInit _ ident -> failure x
  AbsGramatyka.Init _ ident expr -> failure x

transType :: Show a => AbsGramatyka.Type' a -> Result
transType x = case x of
  AbsGramatyka.Int _ -> failure x
  AbsGramatyka.Str _ -> failure x
  AbsGramatyka.Bool _ -> failure x
  AbsGramatyka.Fun _ type_ atypes -> failure x

transAType :: Show a => AbsGramatyka.AType' a -> Result
transAType x = case x of
  AbsGramatyka.FArgV _ type_ -> failure x
  AbsGramatyka.FArgR _ type_ -> failure x

transExpr :: Show a => AbsGramatyka.Expr' a -> Result
transExpr x = case x of
  AbsGramatyka.LamDef _ args type_ fblock -> failure x
  AbsGramatyka.EVar _ ident -> failure x
  AbsGramatyka.ELitInt _ integer -> failure x
  AbsGramatyka.ELitTrue _ -> failure x
  AbsGramatyka.ELitFalse _ -> failure x
  AbsGramatyka.EApp _ ident exprs -> failure x
  AbsGramatyka.EString _ string -> failure x
  AbsGramatyka.Neg _ expr -> failure x
  AbsGramatyka.Not _ expr -> failure x
  AbsGramatyka.EMul _ expr1 mulop expr2 -> failure x
  AbsGramatyka.EAdd _ expr1 addop expr2 -> failure x
  AbsGramatyka.ERel _ expr1 relop expr2 -> failure x
  AbsGramatyka.EAnd _ expr1 expr2 -> failure x
  AbsGramatyka.EOr _ expr1 expr2 -> failure x

transAddOp :: Show a => AbsGramatyka.AddOp' a -> Result
transAddOp x = case x of
  AbsGramatyka.Plus _ -> failure x
  AbsGramatyka.Minus _ -> failure x

transMulOp :: Show a => AbsGramatyka.MulOp' a -> Result
transMulOp x = case x of
  AbsGramatyka.Times _ -> failure x
  AbsGramatyka.Div _ -> failure x
  AbsGramatyka.Mod _ -> failure x

transRelOp :: Show a => AbsGramatyka.RelOp' a -> Result
transRelOp x = case x of
  AbsGramatyka.LTH _ -> failure x
  AbsGramatyka.LE _ -> failure x
  AbsGramatyka.GTH _ -> failure x
  AbsGramatyka.GE _ -> failure x
  AbsGramatyka.EQU _ -> failure x
  AbsGramatyka.NE _ -> failure x
