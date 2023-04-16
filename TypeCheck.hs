{-# LANGUAGE FlexibleInstances #-}
module TypeCheck where

import AbsGramatyka

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
    ( modify, evalStateT, MonadState(get), StateT )
import Data.Map
import LexGramatyka (Token, mkPosToken)
import ParGramatyka (myLexer, pProgram)

-- | The abstract syntax of language Gramatyka. Use () to avoid position information.
type TypeT = Type' ()
type ATypeT = AType' ()

type Var = Ident

type Env = Map Var TypeT 

type Err = String

type TypeMonad a = StateT Env (ExceptT Err Identity) a

unpacType :: Type -> TypeT
unpacType (Int p) = Int ()
unpacType (Str p) = Str ()
unpacType (Bool p) = Bool ()
unpacType (Fun p t ats) = Fun () (unpacType t) (Prelude.map unpacAType ats)

unpacAType :: AType -> ATypeT
unpacAType (FArgV p t) = FArgV () (unpacType t)
unpacAType (FArgR p t) = FArgR () (unpacType t)

runTypeMonad :: TypeMonad a -> Either Err a
runTypeMonad = runIdentity . runExceptT . flip evalStateT empty

typeCheck :: Program -> Either String ()
typeCheck = runTypeMonad . typeCheck'

typeCheck' :: Program -> TypeMonad ()
typeCheck' (Program p stmts) = do
  mapM_ typeCheckStmt stmts

typeCheckStmt :: Stmt -> TypeMonad ()

typeCheckStmt (Decl p t items) = do
  mapM_ (typeCheckItem $ unpacType t) items

typeCheckItem :: TypeT -> Item -> TypeMonad ()
typeCheckItem t (Init p i v) = do
  t' <- typeOf v
  if t == t'
    then do
      modify (insert i t)
    else throwError $ show p ++ " Type mismatch in initialization of " ++ show i ++ ".\nVariable has type " ++ show t ++ " but expression has type " ++ show t' ++ "."

typeOf :: Expr -> TypeMonad TypeT

typeOf (EVar p i) = do
  env <- get
  case Data.Map.lookup i env of
    Just t -> return t
    Nothing -> throwError $ show p ++ " Variable " ++ show i ++ " not in scope"

typeOf (ELitInt p _) = return $ Int ()

instance {-# OVERLAPPING #-} Show BNFC'Position where
  show (Just (l, r)) = "Error at line " ++ show l ++ ",column " ++ show r ++ ":"
  show Nothing = "Error: at unknown position: "