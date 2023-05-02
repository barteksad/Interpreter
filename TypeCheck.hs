{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeCheck where

import AbsGramatyka

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
    ( modify, evalStateT, MonadState(get), StateT )
import Data.Map ( Map, fromList, insert, lookup )
import Control.Monad.State.Lazy

------------------------------------------------------------
-- Types and helper functions
------------------------------------------------------------

-- | The abstract syntax of language Gramatyka. Use () to avoid position information.
type TypeT = Type' ()
type ATypeT = AType' ()

type Var = String

type Env = Map Var TypeT

type Err = String

type TypeMonad a = StateT Env (ExceptT Err Identity) a

initialEnv :: Env = fromList [
  ("printInt", Fun () (Int ()) [FArgV () (Int ())]),
  ("printString", Fun () (Str ()) [FArgV () (Str ())]),
  ("printBool", Fun () (Bool ()) [FArgV () (Bool ())])];

unpacVar :: Ident -> Var
unpacVar (Ident s) = s

unpacType :: Type -> TypeT
unpacType (Int p) = Int ()
unpacType (Str p) = Str ()
unpacType (Bool p) = Bool ()
unpacType (Fun p t ats) = Fun () (unpacType t) (Prelude.map unpacAType ats)

unpacAType :: AType -> ATypeT
unpacAType (FArgV p t) = FArgV () (unpacType t)
unpacAType (FArgR p t) = FArgR () (unpacType t)

unpackArg :: Arg -> ATypeT
unpackArg (VArg p t i) = FArgV () (unpacType t)
unpackArg (RArg p t i) = FArgR () (unpacType t)

unpackIdent :: Ident -> Var
unpackIdent (Ident s) = s

runTypeMonad :: TypeMonad a -> Either Err a
runTypeMonad = runIdentity . runExceptT . flip evalStateT initialEnv

typeCheck :: Program -> Either String ()
typeCheck = runTypeMonad . typeCheck'

typeCheck' :: Program -> TypeMonad ()
typeCheck' (Program p stmts) = do
  mapM_ typeCheckStmt stmts

------------------------------------------------------------
--- Statements
------------------------------------------------------------

typeCheckStmt :: Stmt -> TypeMonad ()
typeCheckStmt (Decl p t items) = do
  mapM_ (typeCheckItem $ unpacType t) items

typeCheckStmt (FnDecl p t i args body) =
  let t' = Fun () (unpacType t) (Prelude.map unpackArg args) in do
    modify $ insert (unpacVar i) t'
    env <- get
    withStateT (addArgs args) (typeCheckFBlock (unpacType t) body)
    put env

typeCheckStmt (Empty _) = return ()

typeCheckStmt (BStmt p (Block _ stmts)) = do
  evn <- get
  mapM_ typeCheckStmt stmts
  put evn

typeCheckStmt (Ass p i e) = do
  t1 <- typeOfIdent p i
  t2 <- typeOf e
  unless (t1 == t2) $
    throwError $ show p ++ " type missmatch! Variable " ++ show i ++ " has type: " ++ show t1 ++ " and expression type is: " ++ show t2

typeCheckStmt (Incr p i) = do
  t <- typeOfIdent p i
  unless (t == Int ()) $
    throwError $ show p ++ " can not increment variable of type: " ++ show t

typeCheckStmt (Decr p i) = do
  t <- typeOfIdent p i
  unless (t == Int ()) $
    throwError $ show p ++ " can not decrement variable of type: " ++ show t

typeCheckStmt (Cond p e stmt) = do
  t <- typeOf e
  unless (t == Bool ()) $
    throwError $ show p ++ " condition must be of type bool but it's type is: " ++ show t
  env <- get
  typeCheckStmt stmt
  put env

typeCheckStmt (CondElse p e stmt1 stmt2) = do
  t <- typeOf e
  unless (t == Bool ()) $
    throwError $ show p ++ " condition must be of type bool but it's type is: " ++ show t
  env <- get
  typeCheckStmt stmt1
  put env
  typeCheckStmt stmt2
  put env

typeCheckStmt (While p e stmt) = do
  t <- typeOf e
  unless (t == Bool ()) $
    throwError $ show p ++ " condition must be of type bool but it's type is: " ++ show t
  env <- get
  typeCheckStmt stmt
  put env

typeCheckStmt (SExp p e) = do
  typeOf e
  return ()

------------------------------------------------------------
--- Other types
------------------------------------------------------------

typeOfIdent :: BNFC'Position -> Ident -> TypeMonad TypeT
typeOfIdent p i = do
  i <- return $ unpacVar i
  env <- get
  case Data.Map.lookup i env of
    Just t1 -> return t1
    Nothing -> throwError $ show p ++ " variable " ++ show i ++ " not declared!"


typeCheckItem :: TypeT -> Item -> TypeMonad ()
typeCheckItem t (Init p i v) = do
  t' <- typeOf v
  i <- return $ unpacVar i
  if t == t'
    then do
      modify $ insert i t
    else throwError $ show p ++ " Type mismatch in initialization of " ++ show i ++ ". Variable has type " ++ show t ++ " but expression has type " ++ show t' ++ "."
typeCheckItem t (NoInit p i) = do
  modify $ insert (unpacVar i) t

typeCheckRet :: TypeT -> Ret -> TypeMonad ()
typeCheckRet t (Ret p v) = do
  t' <- typeOf v
  if t == t'
    then return ()
    else throwError $ show p ++ " Type mismatch in return statement. Function has type " ++ show t ++ " but expression has type " ++ show t' ++ "."

typeCheckFBlock :: TypeT -> FBlock -> TypeMonad ()
typeCheckFBlock t (FBlock p stmts ret) = do
  mapM_ typeCheckStmt stmts
  typeCheckRet t ret

addArgs :: [Arg] -> Env -> Env
addArgs args env = Prelude.foldl (\env arg -> insert (unpacVar $ getArgName arg) (getArgType arg) env) env args

getArgName :: Arg -> Ident
getArgName (VArg p t i) = i
getArgName (RArg p t i) = i

getArgType :: Arg -> TypeT
getArgType (VArg p t i) = unpacType t
getArgType (RArg p t i) = unpacType t

typeCheckArg :: ATypeT -> Expr -> TypeMonad ()
typeCheckArg (FArgV _ t) e = do
  t' <- typeOf e
  unless (t == t') $
    throwError $ show (hasPosition e) ++ "Type missmatch in function call. Expected type " ++ show t ++ " but got type " ++ show t' ++ "."
typeCheckArg (FArgR _ t) (EVar p i) = do
  t' <- typeOfIdent p i
  unless (t == t') $
    throwError $ show p ++ "Type missmatch in function call. Expected type " ++ show t ++ " but got type " ++ show t' ++ "."
typeCheckArg (FArgR _ t) e = do
  throwError $ show (hasPosition e) ++ " Function expects reference but got value."
typeOf :: Expr -> TypeMonad TypeT

typeOf (LamDef _ args rt body) = do
  let t = Fun () (unpacType rt) (Prelude.map unpackArg args)
  env <- get
  withStateT (addArgs args) (typeCheckFBlock (unpacType rt) body)
  put env
  return t

typeOf (EVar p i) = do
  i <- return $ unpacVar i
  env <- get
  case Data.Map.lookup i env of
    Just t -> return t
    Nothing -> throwError $ show p ++ " Variable " ++ show i ++ " not in scope"

typeOf (ELitInt _ _) = return $ Int ()
typeOf (ELitTrue _) = return $ Bool ()
typeOf (ELitFalse _) = return $ Bool ()

typeOf (EApp p i args) = do
  t <- typeOfIdent p i
  case t of
    Fun _ rt ats -> do
      unless (length args == length ats) $
        throwError $ show p ++ " Wrong number of arguments in function call to " ++ show (unpackIdent i) ++ ".\nFunction expects " ++ show (length ats) ++ " arguments but " ++ show (length args) ++ " were given."
      mapM_ (uncurry typeCheckArg) (zip ats args)
      return rt
    _ -> throwError $ show p ++ " Trying to call variable: " ++ show (unpackIdent i) ++ " which is not a function!"

typeOf (EString _ _) = return $ Str ()

typeOf (Neg p e) = do
  t <- typeOf e
  unless (t == Int ()) $
    throwError $ show p ++ " Only integer values can be negated. Expression has type: " ++ show t
  return $ Int ()

typeOf (Not p e) = do
  t <- typeOf e
  unless (t == Bool ()) $
    throwError $ show p ++ " Only boolean values can be negated. Expression has type: " ++ show t
  return $ Bool ()

typeOf (EMul p e1 op e2) = do
  t1 <- typeOf e1
  t2 <- typeOf e2
  unless (t1 == Int ()) $
    throwError $ show p ++ " Left operand of multiplication must be of type int but it's type is: " ++ show t1
  unless (t2 == Int ()) $
    throwError $ show p ++ " Right operand of multiplication must be of type int but it's type is: " ++ show t2
  return $ Int ()

typeOf (EAdd p e1 op e2) = do
  t1 <- typeOf e1
  t2 <- typeOf e2
  unless (t1 == t2) $
    throwError $ show p ++ " Left and right operands of addition must be of the same type but they are: " ++ show t1 ++ " and " ++ show t2
  unless (t1 == Int () || t1 == Str ()) $
    throwError $ show p ++ " Left operand of addition must be of type int or string but it's type is: " ++ show t1
  unless (t2 == Int () || t1 == Str ()) $
    throwError $ show p ++ " Right operand of addition must be of type int or string but it's type is: " ++ show t2
  return $ t1

typeOf (ERel p e1 op e2) = do
  t1 <- typeOf e1
  t2 <- typeOf e2
  unless (t1 == Int ()) $
    throwError $ show p ++ " Left operand of relation must be of type int but it's type is: " ++ show t1
  unless (t2 == Int ()) $
    throwError $ show p ++ " Right operand of relation must be of type int but it's type is: " ++ show t2
  return $ Bool ()

typeOf (EAnd p e1 e2) = do
  t1 <- typeOf e1
  t2 <- typeOf e2
  unless (t1 == Bool ()) $
    throwError $ show p ++ " Left operand of and must be of type bool but it's type is: " ++ show t1
  unless (t2 == Bool ()) $
    throwError $ show p ++ " Right operand of and must be of type bool but it's type is: " ++ show t2
  return $ Bool ()

typeOf (EOr p e1 e2) = do
  t1 <- typeOf e1
  t2 <- typeOf e2
  unless (t1 == Bool ()) $
    throwError $ show p ++ " Left operand of or must be of type bool but it's type is: " ++ show t1
  unless (t2 == Bool ()) $
    throwError $ show p ++ " Right operand of or must be of type bool but it's type is: " ++ show t2
  return $ Bool ()

---------------------
--- Printing stuff ---
---------------------

instance {-# OVERLAPPING #-} Show TypeT where
  show (Int ()) = "int"
  show (Bool ()) = "bool"
  show (Str ()) = "string"
  show (Fun () t args) = show t ++ "(" ++ show args ++ ")"

instance {-# OVERLAPPING #-} Show ATypeT where
  show (FArgV _ t) = show t
  show (FArgR _ t) = "ref " ++ show t

instance {-# OVERLAPPING #-} Show BNFC'Position where
  show (Just (l, r)) = "Error at line " ++ show l ++ ",column " ++ show r ++ ":"
  show Nothing = "Error: at unknown position: "