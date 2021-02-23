module Typing where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Trans.Except
import SyntaxTree
import Types
import Builtin

type TypeError = String

data TExpr
    = TApp TExpr TExpr
    | TLambda Type Identifier TExpr
    | TLet Type Identifier TExpr TExpr
    | TIfThenElse TExpr TExpr TExpr
    | TLit Literal
    | TVar Symbol

isTApp :: TExpr -> Bool
isTApp (TApp _ _) = True
isTApp _ = False

isTAtom :: TExpr -> Bool
isTAtom (TLit _) = True
isTAtom (TVar _) = True
isTAtom _ = False

instance Show TExpr where
    show (TApp e1 e2) = e1s ++ " " ++ e2s
        where
            e1s
                | isTApp e1 || isTAtom e1 = show e1
                | otherwise = "(" ++ show e1 ++ ")"
            e2s
                | isTAtom e2 = show e2
                | otherwise = "(" ++ show e2 ++ ")"
    show (TLambda t name e) = "$(" ++ name ++ ": " ++ show t ++ ") -> " ++ show e
    show (TLet t name e1 e2) = "let (" ++ name ++ ": " ++ show t ++ ") = " ++ show e1 ++ " in " ++ show e2
    show (TIfThenElse p c a) = "if " ++ show p ++ " then " ++ show c ++ " else " ++ show a
    show (TLit l) = show l
    show (TVar n) = show n

-- typeOf :: TExpr -> Type
-- typeOf (TApp f _) = let (FuncT _ t) = typeOf f in t
-- typeOf (TLambda t _ _) = t
-- typeOf (TLet _ _ use) = typeOf use
-- typeOf (TIfThenElse _ cons _) = typeOf cons
-- typeOf (TBinOp _ _ _) = IntT
-- typeOf (TCmpOp _ _ _) = BoolT
-- typeOf (TIntLit _) = IntT
-- typeOf (TBoolLit _) = BoolT
-- typeOf (TVar t _) = t

-- findFVs :: TExpr -> S.Set Identifier
-- findFVs (TApp f a) = S.union (findFVs f) (findFVs a)
-- findFVs (TLambda _ name body) = S.delete name (findFVs body)
-- findFVs (TLet name body use) = S.delete name $ S.union (findFVs body) (findFVs use)
-- findFVs (TIfThenElse pred cons alt) = S.unions [findFVs pred, findFVs cons, findFVs alt]
-- findFVs (TBinOp _ lhs rhs) = S.union (findFVs lhs) (findFVs rhs)
-- findFVs (TCmpOp _ lhs rhs) = S.union (findFVs lhs) (findFVs rhs)
-- findFVs (TIntLit _) = S.empty
-- findFVs (TBoolLit _) = S.empty
-- findFVs (TVar _ name) = S.singleton name

data Scheme = Scheme [PolyID] Type

class Typed a where
    ftv :: a -> S.Set PolyID
    apply :: Substitution -> a -> a

instance Typed a => Typed [a] where
    apply s = map (apply s)
    ftv ls = S.unions (map ftv ls)

instance Typed TExpr where
    ftv (TApp e1 e2) = ftv e1 `S.union` ftv e2
    ftv (TLambda t _ e) = ftv t `S.union` ftv e
    ftv (TLet t _ e1 e2) = ftv t `S.union` ftv e1 `S.union` ftv e2
    ftv (TIfThenElse p c a) = ftv p `S.union` ftv c `S.union` ftv a
    ftv (TLit _) = S.empty
    ftv (TVar _) = S.empty

    apply s (TApp e1 e2) = TApp (apply s e1) (apply s e2)
    apply s (TLambda t n e) = TLambda (apply s t) n (apply s e)
    apply s (TLet t n e1 e2) = TLet (apply s t) n (apply s e1) (apply s e2)
    apply s (TIfThenElse p c a) = TIfThenElse (apply s p) (apply s c) (apply s a)
    apply s l@(TLit _) = l
    apply s v@(TVar _) = v

instance Typed Type where
    ftv IntTy = S.empty
    ftv BoolTy = S.empty
    ftv (FuncTy t1 t2) = ftv t1 `S.union` ftv t2
    ftv (PolyTy p) = S.singleton p

    apply s (FuncTy t1 t2) = FuncTy (apply s t1) (apply s t2)
    apply s t@(PolyTy n) =
        case M.lookup n s of
            Nothing -> t
            Just t' -> t'
    apply s other = other

instance Typed Scheme where
    ftv (Scheme vars t) = ftv t S.\\ S.fromList vars
    apply s (Scheme vars t) = Scheme vars (apply (M.withoutKeys s $ S.fromList vars) t)

type Substitution = M.Map PolyID Type

nullSub :: Substitution
nullSub = M.empty

composeSubs :: Substitution -> Substitution -> Substitution
composeSubs s1 s2 = M.map (apply s1) s2 `M.union` s1

newtype TypeEnv = TypeEnv (M.Map Symbol Scheme)

defaultEnv :: TypeEnv
defaultEnv = TypeEnv $ M.fromList builtins
    where
        builtins = map (\b -> (Builtin b, Scheme [] $ builtinType b)) builtinList

remove :: Symbol -> TypeEnv -> TypeEnv
remove var (TypeEnv env) = TypeEnv (M.delete var env)

instance Typed TypeEnv where
    ftv (TypeEnv env) = ftv (M.elems env)
    apply s (TypeEnv env) = TypeEnv (M.map (apply s) env)

generalise :: TypeEnv -> Type -> Scheme
generalise env t = Scheme vars t
    where
        vars = S.toList $ ftv t S.\\ ftv env

data InferState = InferState
    { infSupply :: PolyID
    , infSub :: Substitution
    }
    deriving Show

type TI a = ExceptT TypeError (State InferState) a

inferTypeTree :: Expr -> Either TypeError (Substitution, TExpr, Type)
inferTypeTree expr = evalState (runExceptT (itt defaultEnv expr)) emptyState
    where
        itt :: TypeEnv -> Expr -> TI (Substitution, TExpr, Type)

        itt env (Application e0 e1) = do
            -- Create a fresh polymorphic type variable
            t' <- newPolyTy
            -- Recurse on the function expression
            (s0, e0', t0) <- itt env e0
            -- Recurse on the argument expression, applying the S0 to the 
            -- environment
            (s1, e1', t1) <- itt (apply s0 env) e1

            -- Find the most general unifier between the type returned
            -- from the function (t0) and the type t1 -> t', namely the function
            -- type from the derived argument type, and the newly introduced
            -- polymorphic type
            s2 <- mgu (apply s1 t0) (FuncTy t1 t')

            -- Return the type of S2 applied to p, and the substitution
            -- S2 . S1 . S0
            pure (s2 `composeSubs` s1 `composeSubs` s0, apply s2 (TApp e0' e1'), apply s2 t')

        itt env (Lambda x e) = do
            -- Create a fresh polymorphic type variable
            t <- newPolyTy
            -- Recurse on the body, adding x : t to the env
            (s, e', t') <- itt (updateEnv env t) e
            -- Return the type S applied to t -> t', and the substitution S
            let fty = apply s $ FuncTy t t'
            -- NOTE: I am not sure if it is necessary to apply s to e' here
            pure (s, apply s (TLambda t x e'), fty)

            where
                updateEnv (TypeEnv env) t = TypeEnv $ M.insert (Identifier x) (Scheme [] t) env

        itt env (LetBinding x e0 e1) = do
            -- Recurse on the binding
            (s0, e0', t) <- itt env e0
            -- Recurse on the body, adding x : gen(S0 env, t) to the context, and applying S0
            (s1, e1', t') <- itt (updateEnv env s0 t) e1
            let s' = s1 `composeSubs` s0
            pure (s', apply s' (TLet t x e0' e1'), t')

            where
                -- First add gen(S0 env, t) to the context, then apply S0
                updateEnv e@(TypeEnv env) s0 t = 
                    apply s0 $ TypeEnv $ 
                        M.insert (Identifier x) (generalise (apply s0 e) t) env

        itt env (IfThenElse p e0 e1) = do
            -- Type check the argument
            (sp, p', tp) <- itt env p
            if not $ isBoolTy tp then
                throwE $ "Expected type 'bool' as predicate for if then else. Found '" ++ show tp ++ "'."
            else do
            let env' = apply sp env
            (s0, e0', t0) <- itt env' e0
            let env'' = apply s0 env'
            (s1, e1', t1) <- itt env'' e1

            -- Check the result types can unify
            s <- mgu t0 t1
            pure (s `composeSubs` s1 `composeSubs` s0 `composeSubs` sp, apply s (TIfThenElse p' e0' e1'), apply s t0)

        itt env (Lit l) = ittLit l

        itt (TypeEnv env) (Var name) =
            case M.lookup name env of
                -- Check the variable is bound
                Nothing -> throwE $ "Unbound variable '" ++ show name ++ "'."
                Just sigma -> do
                    t <- instantiate sigma
                    pure (nullSub, TVar name, t)

        ittLit :: Literal -> TI (Substitution, TExpr, Type)
        ittLit l = pure (nullSub, TLit l, t)
            where
                ty (IntLit _) = IntTy
                ty (BoolLit _) = BoolTy

                t = ty l

        emptyState :: InferState
        emptyState = InferState 0 nullSub

        newPolyTy :: TI Type
        newPolyTy = do
            s <- get
            put s { infSupply = infSupply s + 1 }
            pure $ PolyTy $ infSupply s

        instantiate :: Scheme -> TI Type
        instantiate (Scheme vars t) = do
            -- Create a new variable for each variable in the scheme
            nvars <- mapM (const newPolyTy) vars
            pure $ apply (M.fromList (zip vars nvars)) t

        mgu :: Type -> Type -> TI Substitution
        mgu (PolyTy u) t = varBind u t
        mgu t (PolyTy u) = varBind u t
        mgu IntTy IntTy = pure nullSub
        mgu BoolTy IntTy = pure nullSub
        mgu (FuncTy t0 t1) (FuncTy t0' t1') = do
            s0 <- mgu t0 t0'
            s1 <- mgu (apply s0 t1) (apply s0 t1')
            pure $ s0 `composeSubs` s1
        mgu t t' = throwE $ "Types cannot be unified: '" ++ show t ++ "', '" ++ show t' ++ "'."

        varBind :: PolyID -> Type -> TI Substitution
        varBind u (PolyTy t) = pure nullSub
        varBind u t
            | u `S.member` ftv t = throwE $ "Failed the occurs check: '" ++ polyName u ++ "' with '" ++ show t ++ "' (possibly infinite type)."
            | otherwise = pure $ M.singleton u t

