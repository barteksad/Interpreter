module Run where

import AbsGramatyka qualified as Abs
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
  ( MonadState (get),
    StateT,
    evalStateT,
    modify,
  )
import Control.Monad.State.Lazy
import Data.Map

------------------------------------------------------------
-- Types and helper functions
------------------------------------------------------------

type Output = [String]

type Var = String

type Loc = Int

type Env = Map Var Loc

data FArg = VArg Var | RArg Var

data Fun
  = Fun
      { fargs :: [FArg],
        fbody :: Abs.FBlock,
        fenv :: Env
      }
  | Empty | PrintInt | PrintString | PrintBool

data Value
  = I Integer
  | Str String
  | B Bool
  | F Fun
  | L Loc

defaultV :: Abs.Type -> Value
defaultV (Abs.Int p) = I 0
defaultV (Abs.Str p) = Str ""
defaultV (Abs.Bool p) = B False
defaultV (Abs.Fun p t ats) = F Empty

type VState = Map Loc Value

newLoc :: VState -> Loc
newLoc = size

data PState = PState
  { penv :: Env,
    pstate :: VState,
    poutput :: Output
  }

type Err = String

type PStateMonad a = StateT PState (ExceptT Err Identity) a

unpackIdent :: Abs.Ident -> Var
unpackIdent (Abs.Ident s) = s

unpackArg :: Abs.Arg -> FArg
unpackArg (Abs.VArg p t i) = VArg (unpackIdent i)
unpackArg (Abs.RArg p t i) = RArg (unpackIdent i)

initialEnv :: Env =
  fromList
    [ ("printInt", 0),
      ("printString", 1),
      ("printBool", 2)
    ]

initialPState :: PState =
  PState
    { penv = initialEnv,
      pstate = fromList [(0, F PrintInt), (1, F PrintString), (2, F PrintBool)],
      poutput = []
    }

modifyState :: (Map Loc Value -> Map Loc Value) -> PStateMonad ()
modifyState f = do
  state <- gets pstate
  modify (\s -> s {pstate = f state})

modifyEnv :: (Map Var Int -> Map Var Int) -> PStateMonad ()
modifyEnv f = do
  env <- gets penv
  modify (\s -> s {penv = f env})

getVarLoc :: Var -> PStateMonad Loc
getVarLoc var = do
  env <- gets penv
  maybe undefined return (Data.Map.lookup var env)

getLocVal :: Loc -> PStateMonad Value
getLocVal loc = do
  state <- gets pstate
  maybe undefined return (Data.Map.lookup loc state)

setLocVar :: Loc -> Value -> PStateMonad ()
setLocVar loc val = modifyState (insert loc val)

setVarLoc :: Var -> Loc -> PStateMonad ()
setVarLoc var loc = modifyEnv (insert var loc)

getVarVal :: Var -> PStateMonad Value
getVarVal var = do
  loc <- getVarLoc var
  getLocVal loc

setVarVal :: Var -> Value -> PStateMonad ()
setVarVal var val = do
  loc <- getVarLoc var
  setLocVar loc val

runPStateMonad :: PStateMonad a -> Either Err a
runPStateMonad = runIdentity . runExceptT . flip evalStateT initialPState

run :: Abs.Program -> Either String Output
run = runPStateMonad . run'

run' :: Abs.Program -> PStateMonad Output
run' (Abs.Program p stmts) = do
  mapM_ runStmt stmts
  gets poutput

------------------------------------------------------------
--- Statements
------------------------------------------------------------

runStmt :: Abs.Stmt -> PStateMonad ()

runStmt (Abs.Decl p t items) = do
  mapM_ (runDeclItem t) items

runStmt (Abs.FnDecl p t i args body) = do
  pstate <- gets pstate
  let var = unpackIdent i
  let loc = newLoc pstate
  modifyEnv (insert var loc)
  env <- gets penv
  modifyState (insert loc (F (Fun (Prelude.map unpackArg args) body env)))

runStmt (Abs.Empty _) = return ()

runStmt (Abs.BStmt p (Abs.Block _ stmts)) = do
  penv <- gets penv
  mapM_ runStmt stmts
  modify (\s -> s {penv = penv})

runStmt (Abs.Ass p i e) = do
  v <- runExp e
  setVarVal (unpackIdent i) v

runStmt (Abs.Incr p i) = do
  v <- getVarVal (unpackIdent i)
  setVarVal (unpackIdent i) (v + 1)

runStmt (Abs.Decr p i) = do
  v <- getVarVal (unpackIdent i)
  setVarVal (unpackIdent i) (v - 1)

runStmt (Abs.Cond p e stmt) = do
  cond <- runExp e
  when (toBool cond) $ do
    penv <- gets penv
    runStmt stmt
    modify (\s -> s {penv = penv})

runStmt (Abs.CondElse p e stmt1 stmt2) = do
  cond <- runExp e
  penv <- gets penv
  if toBool cond
    then do
      runStmt stmt1
    else do
      runStmt stmt2
  modify (\s -> s {penv = penv})

runStmt (Abs.While p e stmt) = do
  cond <- runExp e
  penv <- gets penv
  when (toBool cond) $ do
    runStmt stmt
    withStateT (\s -> s {penv = penv}) (runStmt (Abs.While p e stmt))
    modify (\s -> s {penv = penv})

runStmt (Abs.SExp p e) = do
  _ <- runExp e
  return ()

------------------------------------------------------------
--- Expressions
------------------------------------------------------------

runExp :: Abs.Expr -> PStateMonad Value

runExp (Abs.LamDef p arg t body) = do
  penv <- gets penv
  pstate <- gets pstate
  return (F (Fun [] body penv))

runExp (Abs.EVar p i) = getVarVal (unpackIdent i)

runExp (Abs.ELitInt p i) = return (I i)
runExp (Abs.ELitTrue p) = return (B True)
runExp (Abs.ELitFalse p) = return (B False)
runExp (Abs.EString p s) = return (Str s)

runExp (Abs.Neg p e) = do
  v <- runExp e
  return (-v)

runExp (Abs.Not p e) = do
  v <- runExp e
  return (B ((not . toBool)  v))

runExp (Abs.EMul p e1 op e2) = do
  v1 <- runExp e1
  v2 <- runExp e2
  case op of
    Abs.Times _ -> return (v1 * v2)
    Abs.Div _ -> do
      unless (v2 /= 0) $ do
        throwError $ show p ++ " Division by zero!"
      return (v1 / v2)
    Abs.Mod _ -> return (v1 - v1 / v2 * v2)

runExp (Abs.EAdd p e1 op e2) = do
  v1 <- runExp e1
  v2 <- runExp e2
  case op of
    Abs.Plus _ -> return (v1 + v2)
    Abs.Minus _ -> return (v1 - v2)

runExp (Abs.ERel p e1 op e2) = do
  v1 <- runExp e1
  v2 <- runExp e2
  case op of
    Abs.LTH _ -> return (B (v1 < v2))
    Abs.LE _ -> return (B (v1 <= v2))
    Abs.GTH _ -> return (B (v1 > v2))
    Abs.GE _ -> return (B (v1 >= v2))
    Abs.EQU _ -> return (B (v1 == v2))
    Abs.NE _ -> return (B (v1 /= v2))

runExp (Abs.EAnd p e1 e2) = do
  v1 <- runExp e1
  v2 <- runExp e2
  return (B (toBool v1 && toBool v2))

runExp (Abs.EOr p e1 e2) = do
  v1 <- runExp e1
  v2 <- runExp e2
  return (B (toBool v1 || toBool v2))

runExp (Abs.EApp p i args) = do
  pstate <- gets pstate
  val <- getVarVal (unpackIdent i)
  case val of
    F (Fun args' body env') -> do
      penv <- gets penv
      setFArgsAndEnv args' args env'
      retVal <- runFBlock body
      modify (\s -> s {penv = penv})
      return retVal
    F PrintInt -> do
      v <- runExp (head args)
      modify (\s -> s {poutput = show v : poutput s})
      return (I 0)
    F PrintString -> do
      v <- runExp (head args)
      modify (\s -> s {poutput = show v : poutput s})
      return (Str "")
    F PrintBool -> do
      v <- runExp (head args)
      modify (\s -> s {poutput = show v : poutput s})
      return (B False)
------------------------------------------------------------
--- Other types
------------------------------------------------------------

runDeclItem :: Abs.Type -> Abs.Item -> PStateMonad ()

runDeclItem t (Abs.Init p i e) = do
  v <- runExp e
  pstate <- gets pstate
  let var = unpackIdent i
  let loc = newLoc pstate
  modifyEnv (insert var loc)
  modifyState (insert loc v)

runDeclItem t (Abs.NoInit p i) = do
  pstate <- gets pstate
  let var = unpackIdent i
  let loc = newLoc pstate
  modifyEnv (insert var loc)
  modifyState (insert loc (defaultV t))

runFBlock :: Abs.FBlock -> PStateMonad Value
runFBlock (Abs.FBlock _ stmts (Abs.Ret _ e)) = do
  mapM_ runStmt stmts
  runExp e

setFArgsAndEnv :: [FArg] -> [Abs.Expr] -> Env -> PStateMonad ()
setFArgsAndEnv args exps fenv = do
  values <- zipWithM runFArgExp args exps
  modifyEnv (const fenv)
  mapM_ (uncurry setFArg) (zip args values)

runFArgExp :: FArg -> Abs.Expr -> PStateMonad Value

runFArgExp (VArg _) e = runExp e

runFArgExp (RArg _) (Abs.EVar _ var) = do
  l <- getVarLoc (unpackIdent var)
  return (L l)

setFArg :: FArg -> Value -> PStateMonad ()

setFArg (VArg var) v = do
  pstate <- gets pstate
  let loc = newLoc pstate
  modifyEnv (insert var loc)
  modifyState (insert loc v)

setFArg (RArg var) (L l) = do
  setVarLoc var l

------------------------------------------------------------
--- Utils
------------------------------------------------------------

toBool :: Value -> Bool
toBool (B True) = True
toBool (B False) = False

instance Num Value where
  (I a) + (I b) = I (a + b)
  (Str a) + (Str b) = Str (a ++ b)
  (I a) - (I b) = I (a - b)
  (I a) * (I b) = I (a * b)
  abs (I a) = I (abs a)
  signum (I a) = I (signum a)
  fromInteger a = I (fromInteger a)
  negate (I a) = I (negate a)

instance Fractional Value where
  (I a) / (I b) = I (a `div` b)
  fromRational = undefined
  recip = undefined

instance Ord Value where
  (I a) < (I b) = a < b
  (I a) <= (I b) = a <= b
  (I a) > (I b) = a > b
  (I a) >= (I b) = a >= b

instance Eq Value where
  (I a) == (I b) = a == b
  (Str a) == (Str b) = a == b
  (B a) == (B b) = a == b
  _ == _ = False

instance Show Value where
  show (I a) = show a
  show (Str a) = a
  show (B a) = show a

instance {-# OVERLAPPING #-} Show Abs.BNFC'Position where
  show (Just (l, r)) = "Error at line " ++ show l ++ ",column " ++ show r ++ ":"
  show Nothing = "Error: at unknown position: "