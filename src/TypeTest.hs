module TypeTest where

import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)

type Identifier = String

data Lit
    = Int Int
    | Bool Bool
    deriving Show

data Exp
    = App Exp Exp
    | Lam Identifier Exp
    | Let Identifier Exp Exp
    | Lit Lit
    | Var Identifier
    deriving Show

data Type
    = PolyT String
    | IntT
    | BoolT
    | FuncT Type Type
    deriving Show

data Scheme = Scheme [Identifier] Type

data TExp
    = TApp TExp TExp
    | TLam Type Identifier TExp
    | TLet Type Identifier TExp TExp
    | TLit Lit
    | TVar Identifier
    deriving Show

instance Types TExp where
    ftv (TApp e1 e2) = ftv e1 `S.union` ftv e2
    ftv (TLam t _ e) = ftv t `S.union` ftv e
    ftv (TLet t _ e1 e2) = ftv t `S.union` ftv e1 `S.union` ftv e2
    ftv (TLit _) = S.empty
    ftv (TVar _) = S.empty

    apply s (TApp e1 e2) = TApp (apply s e1) (apply s e2)
    apply s (TLam t n e) = TLam (apply s t) n (apply s e)
    apply s (TLet t n e1 e2) = TLet (apply s t) n (apply s e1) (apply s e2)
    apply s l@(TLit _) = l
    apply s v@(TVar _) = v

class Types a where
    ftv :: a -> S.Set Identifier
    apply :: Substitution -> a -> a

instance Types Type where
    ftv (PolyT n) = S.singleton n
    ftv IntT = S.empty
    ftv BoolT = S.empty
    ftv (FuncT from to) = S.union (ftv from) (ftv to)

    apply s t@(PolyT n) =
        case M.lookup n s of
            Nothing -> t
            Just t' -> t'
    apply s (FuncT from to) = FuncT (apply s from) (apply s to)
    apply s t = t

instance Types Scheme where
    ftv (Scheme vars t) = ftv t S.\\ S.fromList vars
    apply s (Scheme vars t) = Scheme vars (apply (foldr M.delete s vars) t)

instance Types a => Types [a] where
    apply s = map (apply s)
    ftv l = S.unions (map ftv l)

type Substitution = M.Map Identifier Type

nullSub :: Substitution
nullSub = M.empty

composeSubs :: Substitution -> Substitution -> Substitution
composeSubs s1 s2 = (M.map (apply s1) s2) `M.union` s1

newtype TypeEnv = TypeEnv (M.Map String Scheme)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (M.delete var env)

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (M.elems env)
    apply s (TypeEnv env) = TypeEnv (M.map (apply s) env)

generalise :: TypeEnv -> Type -> Scheme
generalise env t = Scheme vars t
    where
        vars = S.toList ((ftv t) S.\\ (ftv env))

data TIState = TIState
    { tiSupply :: Int
    , tiSub :: Substitution
    }
    deriving Show

type TI a = ExceptT String (State TIState) a

runTI :: TI a -> (Either String a, TIState)
runTI t = runState (runExceptT t) initTIState
    where
        initTIState = TIState
            { tiSupply = 0
            , tiSub = M.empty
            }

newTyVar :: String -> TI Type
newTyVar prefix = do
    s <- get
    put s { tiSupply = tiSupply s + 1 }
    pure $ PolyT (prefix ++ show (tiSupply s))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
    nvars <- mapM (const $ newTyVar "a") vars
    pure $ apply (M.fromList (zip vars nvars)) t

mgu :: Type -> Type -> TI Substitution
mgu (FuncT l r) (FuncT l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    pure $ s1 `composeSubs` s2
mgu (PolyT u) t = varBind u t
mgu t (PolyT u) = varBind u t
mgu IntT IntT  = pure nullSub
mgu BoolT BoolT = pure nullSub
mgu t1 t2 = throwE $ "Types cannot be unified: '" ++ show t1 ++ "', '" ++ show t2 ++ "'."

varBind :: String -> Type -> TI Substitution
varBind u (PolyT t) = pure nullSub
varBind u t
    | u `S.member` ftv t = throwE $ "Failed the occurrs check: '" ++ u ++ "' vs. '" ++ show t ++ "'."
    | otherwise = pure $ M.singleton u t

tiLit :: TypeEnv -> Lit -> TI (Substitution, TExp, Type)
tiLit _ l@(Int _) = pure (nullSub, TLit l, IntT)
tiLit _ l@(Bool _) = pure (nullSub, TLit l, BoolT)

ti :: TypeEnv -> Exp -> TI (Substitution, TExp, Type)
ti (TypeEnv env) (Var n) =
    case M.lookup n env of
        Nothing -> throwE $ "Unbound variable: '" ++ n ++ "'."
        Just sigma -> do
            t <- instantiate sigma
            pure (nullSub, TVar n, t)

ti env (Lit l) = tiLit env l

ti env (Lam n e) = do
    tv <- newTyVar "a"
    let TypeEnv env' = remove env n
        env'' = TypeEnv (env' `M.union` (M.singleton n (Scheme [] tv)))
    (s1, e', t1) <- ti env'' e
    let argTy = apply s1 tv
    pure (s1, TLam argTy n e', FuncT argTy t1)

ti env (App e1 e2) = do
    tv <- newTyVar "a"
    (s1, e1', t1) <- ti env e1
    (s2, e2', t2) <- ti (apply s1 env) e2
    s3 <- mgu (apply s2 t1) (FuncT t2 tv)
    pure (s3 `composeSubs` s2 `composeSubs` s1, apply s3 (TApp e1' e2'), apply s3 tv)

ti env (Let x e1 e2) = do
    (s1, e1', t1) <- ti env e1
    let TypeEnv env' = remove env x
        t' = generalise (apply s1 env) t1
        env'' = TypeEnv (M.insert x t' env')
    (s2, e2', t2) <- ti (apply s1 env'') e2
    pure (s1 `composeSubs` s2, TLet t1 x e1' e2', t2)

typeInference :: M.Map Identifier Scheme -> Exp -> TI (TExp, Type)
typeInference env e = do
    (s, e', t) <- ti (TypeEnv env) e
    pure (apply s e', apply s t)

e0 = Let "id" (Lam "x" (Var "x")) (Var "id")
e1 = Let "id" (Lam "x" (Var "x")) (App (Var "id") (Var "id"))
e2 = Let "id" (Lam "x" (Let "y" (Var "x") (Var "y"))) (App (Var "id") (Var "id"))
e3 = Let "id" (Lam "x" (Let "y" (Var "x") (Var "y"))) (App (App (Var "id") (Var "id")) (Lit (Int 2)))
e4 = Let "id" (Lam "x" (App (Var "x") (Var "x"))) (Var "id")
e5 = Lam "m" (Let "y" (Var "m") (Let "x" (App (Var "y") (Lit (Bool True))) (Var "x")))
