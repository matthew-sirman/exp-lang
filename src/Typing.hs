module Typing (
    TypeError
  , inferTypeTree

  , module Types
) where

import qualified DataStructs.HashMap as M
import qualified DataStructs.HashSet as S
import Control.Monad.State
import Control.Monad.Trans.Except
import SyntaxTree
import Types

import Debug.Trace

-- Create an alias of string for a type error
type TypeError = String

-- The following is an implementation of Algorithm W for type inference
-- and checking of the Hindley-Milner type system.

-- Represents a type scheme - a type containing a list of variables bound
-- by universal quantifiers
-- e.g. forall a. a -> a
data Scheme = Scheme [PolyID] Type

-- Typeclass for useful functions on entities which can be considerd
-- in some regard as "typed".
class Typed a where
    -- Returns a set of free type variables from the given type
    ftv :: a -> S.HashSet PolyID
    -- Applies a type substitution to a typed entity. A substitution
    -- is defined as a finite mapping from polymorphic type names to
    -- concrete types
    apply :: Substitution -> a -> a

-- Prove a list of typed items can also be typed
instance Typed a => Typed [a] where
    -- Take the union of the type variables
    ftv ls = S.unions (map ftv ls)
    -- Apply the substitution across the list
    apply s = map (apply s)

-- Prove a typed expression is typed
instance Typed t => Typed (Expr t) where
    -- The free type variables are as expected - just collected
    -- from the type annotations in the tree
    -- Note: we only need to collect type variables from
    -- lambda and let bindings - every other variable
    -- references one of these.
    -- Annotations on applications and vars are useful
    -- later when it is hard to infer the type properly
    ftv (App _ e1 e2) = ftv e1 `S.union` ftv e2
    ftv (Lam t _ e) = ftv t `S.union` ftv e
    ftv (Let t _ e1 e2) = ftv t `S.union` ftv e1 `S.union` ftv e2
    ftv (LetRec t _ e1 e2) = ftv t `S.union` ftv e1 `S.union` ftv e2
    ftv (IfThenElse p c a) = ftv p `S.union` ftv c `S.union` ftv a
    ftv (Lit _) = S.empty
    ftv (Pair l r) = ftv l `S.union` ftv r
    ftv (Var _ _) = S.empty

    -- Application is also as expected - just applied to each
    -- type annotation in the tree
    apply s (App t e1 e2) = App (apply s t) (apply s e1) (apply s e2)
    apply s (Lam t n e) = Lam (apply s t) n (apply s e)
    apply s (Let t n e1 e2) = Let (apply s t) n (apply s e1) (apply s e2)
    apply s (LetRec t n e1 e2) = LetRec (apply s t) n (apply s e1) (apply s e2)
    apply s (IfThenElse p c a) = IfThenElse (apply s p) (apply s c) (apply s a)
    apply s l@(Lit _) = l
    apply s (Pair l r) = Pair (apply s l) (apply s r)
    apply s (Var t n) = Var (apply s t) n

-- Prove a type itself is typed
instance Typed Type where
    -- The free type variables are collected as expected - collecting
    -- a set of all the PolyTy variables
    ftv UnitTy = S.empty
    ftv IntTy = S.empty
    ftv BoolTy = S.empty
    ftv (FuncTy t1 t2) = ftv t1 `S.union` ftv t2
    ftv (PairTy l r) = ftv l `S.union` ftv r
    ftv (PolyTy p) = S.singleton p

    -- Application of a substitution is also as expected
    apply s (FuncTy t1 t2) = FuncTy (apply s t1) (apply s t2)
    apply s (PairTy l r) = PairTy (apply s l) (apply s r)
    -- This is the only interesting case here - if the polymorphic
    -- name is in the substitution, update the type variable
    -- Otherwise, leave it
    apply s t@(PolyTy n) =
        case M.lookup n s of
            Nothing -> t
            Just t' -> t'
    apply s other = other

-- Prove type scheme is typed
instance Typed Scheme where
    -- The free variables are any variables which are free in the type itself,
    -- but not bound in the quantifier context
    ftv (Scheme vars t) = ftv t S.\\ S.fromList vars
    -- Application of a substitution doesn't affect the variables, and ignores
    -- any times bound by the scheme
    apply s (Scheme vars t) = Scheme vars (apply (M.withoutKeys s $ S.fromList vars) t)

-- A type substitution, defined as a finite mapping from polymorphic
-- type variables to concrete types
type Substitution = M.HashMap PolyID Type

-- The null substitution is just the empty map
nullSub :: Substitution
nullSub = M.empty

-- The composite of two substitions, writting S1 . S2.
-- We want the property that apply (S1 . S2) t == apply S1 (apply S2 t),
-- so the way we achieve this is by mapping the application of S1 over
-- every value in S2, and then taking the union with S1.
-- This has the effect that any value in S2 has already had the second
-- substitution applied, thus giving the desired composite
composeSubs :: Substitution -> Substitution -> Substitution
composeSubs s1 s2 = M.map (apply s1) s2 `M.union` s1

-- A type environment is just a finite mapping from symbols (i.e. builtins
-- or bound user variables) to type schemes
newtype TypeEnv = TypeEnv (M.HashMap Symbol Scheme)

-- The default environment contains all the builtins mapped to their
-- schemes
defaultEnv :: TypeEnv
defaultEnv = TypeEnv $ M.fromList builtins
    where
        builtins = map (\b -> (Builtin b, Scheme [] $ builtinType b)) builtinList

-- Helper function to remove a symbol from the environment - this just
-- goes under the container of the TypeEnv wrapper
remove :: Symbol -> TypeEnv -> TypeEnv
remove var (TypeEnv env) = TypeEnv (M.delete var env)

-- Proof that an environment is "typed"
instance Typed TypeEnv where
    -- The free variables are just those of the list of elements in the map
    -- (we already defined how to take a list of typed things and get the free
    -- variables)
    ftv (TypeEnv env) = ftv (M.elems env)
    -- Apply the substitution to a type environment; namely
    -- map over the elements - we already proved schemes are typed
    apply s (TypeEnv env) = TypeEnv (M.map (apply s) env)

-- Generalise a type into a scheme
-- This means taking every free variable in the type which is not
-- bound in the type environment, and universally quantifying over them
-- in the form of creating a type scheme.
generalise :: TypeEnv -> Type -> Scheme
generalise env t = Scheme vars t
    where
        vars = S.toList $ ftv t S.\\ ftv env

-- The inference state is just used to track the most
-- recently used polymorphic type variable so we can
-- always construct a new one
newtype InferState = InferState
    { infSupply :: PolyID
    }
    deriving Show

-- This defines the monad stack we use here - the inner monad
-- is a state for generating new type variables, and the outer
-- is an except monad transformer, as type inference
-- may fail and throw an error
type TI a = ExceptT TypeError (State InferState) a

-- Main type inference algorithm. Takes an untyped expression and annotates
-- each term, as well as returning the overall type of the program
inferTypeTree :: Expr () -> Either TypeError (Expr Type, Type)
inferTypeTree expr = dropSubstitution <$> evalState (runExceptT (itt defaultEnv expr)) emptyState
    where
        dropSubstitution (_, te, t) = (te, t)

        itt :: TypeEnv -> Expr () -> TI (Substitution, Expr Type, Type)

        itt env (App () e0 e1) = do
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
            let t = apply s2 t'
            pure (s2 `composeSubs` s1 `composeSubs` s0, apply s2 (App t e0' e1'), t)

        itt env (Lam () x e) = do
            -- Create a fresh polymorphic type variable
            (t, env') <- patternTy x
            -- Recurse on the body, adding x : t to the env
            (s, e', t') <- itt (updateEnv env env') e
            -- Return the type S applied to t -> t', and the substitution S
            let fty = apply s $ FuncTy t t'
            -- NOTE: I am not sure if it is necessary to apply s to e' here
            pure (s, apply s (Lam t x e'), fty)

            where
                updateEnv (TypeEnv env) (TypeEnv env') = TypeEnv $ env `M.union` env'

        itt env (Let () x e0 e1) = do
            -- Recurse on the binding
            (s0, e0', t) <- itt env e0
            env' <- checkPattern (apply s0 env) x t
            -- Recurse on the body, adding x : gen(S0 env, t) to the context, and applying S0
            (s1, e1', t') <- itt (updateEnv s0 env env') e1
            let s' = s1 `composeSubs` s0
            pure (s', apply s' (Let t x e0' e1'), t')

            where
                -- -- First add gen(S0 env, t) to the context, then apply S0
                -- updateEnv e@(TypeEnv env) s0 t = 
                --     apply s0 $ TypeEnv $ 
                --         M.insert (Identifier x) (generalise (apply s0 e) t) env
                updateEnv s0 (TypeEnv e1) (TypeEnv e2) =
                    apply s0 $ TypeEnv (e1 `M.union` e2)

        itt env (LetRec () x e0 e1) = do
            t1 <- newPolyTy
            (s0, e0', t1') <- itt (addType t1 env) e0
            let t1'' = apply s0 t1
            s0' <- mgu t1' t1''
            (s1, e1', t') <- itt (addGenType t1'' env) e1
            let s' = s1 `composeSubs` s0
            pure (s', LetRec t1'' x (apply s' e0') (apply s' e1'), t')

            where
                addType t (TypeEnv e) = TypeEnv $ M.insert (Identifier x) (Scheme [] t) e
                addGenType t (TypeEnv e) = TypeEnv $ M.insert (Identifier x) (generalise env t) e

        itt env (IfThenElse p e0 e1) = do
            -- Type check the argument
            (sp, p', tp) <- itt env p
            if not $ isBoolTy tp then
                throwE $ "Expected type 'bool' as predicate for if then else. Found '" ++ show tp ++ "'."
            else pure ()
            let env' = apply sp env
            (s0, e0', t0) <- itt env' e0
            let env'' = apply s0 env'
            (s1, e1', t1) <- itt env'' e1

            -- Check the result types can unify
            s <- mgu t0 t1
            pure (s `composeSubs` s1 `composeSubs` s0 `composeSubs` sp, apply s (IfThenElse p' e0' e1'), apply s t0)

        -- Outsource the literal case to avoid overly nested patterns
        itt env (Lit l) = ittLit l

        itt env (Pair l r) = do
            (sl, l', tl) <- itt env l
            let env' = apply sl env
            (sr, r', tr) <- itt env' r
            let s = sr `composeSubs` sl
            pure (s, apply s (Pair l' r'), PairTy (apply sr tl) tr)

        -- The variable case just looks up a variable and instantiates it
        itt (TypeEnv env) (Var () name) =
            case M.lookup name env of
                -- Check the variable is bound
                Nothing -> throwE $ "Unbound variable '" ++ show name ++ "'."
                Just sigma -> do
                    t <- instantiate sigma
                    pure (nullSub, Var t name, t)

        -- The literal case always succeeds and returns a type depending
        -- on the literal type
        ittLit :: Literal -> TI (Substitution, Expr Type, Type)
        ittLit l = pure (nullSub, Lit l, t)
            where
                ty (UnitLit) = UnitTy
                ty (IntLit _) = IntTy
                ty (BoolLit _) = BoolTy

                t = ty l

        emptyState :: InferState
        emptyState = InferState 0

        -- Helper function to generate a fresh type variable
        newPolyTy :: TI Type
        newPolyTy = do
            -- Get the current supply of variables
            s <- get
            -- Increment the supplier
            put s { infSupply = infSupply s + 1 }
            -- Return the fresh polymorphic type
            pure $ PolyTy $ infSupply s

        -- Helper function for creating a fresh most general type
        -- for a pattern, and an environment update for this pattern
        patternTy :: Pattern -> TI (Type, TypeEnv)
        patternTy = patternTy' M.empty
            where
                patternTy' env (PVar name) = do
                    -- If the name is already in the pattern env, throw an error
                    if idName `M.member` env then
                        throwE $ "Duplicate usage of identifier '" ++ name ++ "' in pattern."
                    else do
                        -- For a variable, create a fresh type and
                        -- return it, along with the map from the identifier to
                        -- the scheme containing just this type variable
                        t <- newPolyTy
                        pure (t, TypeEnv $ M.insert idName (Scheme [] t) env)
                    where
                        idName = Identifier name
                patternTy' env (PPair l r) = do
                    (tl, TypeEnv el) <- patternTy' env l
                    (tr, env') <- patternTy' el r
                    pure (PairTy tl tr, env')
                patternTy' env (PLit l) = pure (lty l, TypeEnv env)
                    where
                        lty (UnitLit) = UnitTy
                        lty (IntLit _) = IntTy
                        lty (BoolLit _) = BoolTy

        -- Helper function for checking a pattern matches a type, and then
        -- creating a type substitution for this pattern
        checkPattern :: TypeEnv -> Pattern -> Type -> TI TypeEnv
        checkPattern = checkPattern' M.empty
            where
                checkPattern' env e (PVar name) t = 
                    if idName `M.member` env then
                        throwE $ "Duplicate usage of identifier '" ++ name ++ "' in pattern."
                    else
                        pure $ TypeEnv $ M.insert idName (generalise e t) env
                    where
                        idName = Identifier name
                checkPattern' env e (PPair l r) (PairTy tl tr) = do
                    (TypeEnv env') <- checkPattern' env e l tl
                    checkPattern' env' e r tr
                checkPattern' env _ (PLit (UnitLit)) UnitTy = pure $ TypeEnv env
                checkPattern' env _ (PLit (IntLit _)) IntTy = pure $ TypeEnv env
                checkPattern' env _ (PLit (BoolLit _)) BoolTy = pure $ TypeEnv env
                checkPattern' _ _ p t = throwE $ "Could not unify pattern '" ++ show p ++ "' with type '" ++ show t ++ "'."

        -- Instantiate a scheme into a type by creating a fresh type variable
        -- for each type in the scheme, and updating the type with a substitution
        instantiate :: Scheme -> TI Type
        instantiate (Scheme vars t) = do
            -- Create a new variable for each variable in the scheme
            nvars <- mapM (const newPolyTy) vars
            pure $ apply (M.fromList (zip vars nvars)) t

        -- Calculate the most general unifier type between two types
        -- This stage may fail - this indicates the types could not be unified
        -- which means there was a type error.
        mgu :: Type -> Type -> TI Substitution
        mgu (PolyTy u) t = varBind u t
        mgu t (PolyTy u) = varBind u t
        mgu BoolTy BoolTy = pure nullSub
        mgu IntTy IntTy = pure nullSub
        mgu BoolTy IntTy = pure nullSub
        mgu (FuncTy t0 t1) (FuncTy t0' t1') = do
            s0 <- mgu t0 t0'
            s1 <- mgu (apply s0 t1) (apply s0 t1')
            pure $ s0 `composeSubs` s1
        mgu (PairTy l r) (PairTy l' r') = do
            s0 <- mgu l l'
            s1 <- mgu (apply s0 r) (apply s0 r')
            pure $ s0 `composeSubs` s1
        -- The missing cases involve unifications like int with bool, which clearly
        -- fails. Unifiable types must have the same syntactic structure, or one
        -- must be polymorphic and not yet bound
        mgu t t' = throwE $ "Types cannot be unified: '" ++ show t ++ "', '" ++ show t' ++ "'."

        -- Attempt to bind one poly type to another type
        varBind :: PolyID -> Type -> TI Substitution
        -- If the other type is the same polymorphic type, no substitution
        -- is needed
        -- Otherwise, unify u -> t
        varBind u t@(PolyTy u') 
            | u == u' = pure nullSub
            | otherwise = pure $ M.singleton u t
        -- Otherwise, if the poly type 'u' is free in t, there is an infinite
        -- type construction, something like a ~ a -> a
        -- This case fails the occur check.
        -- Otherwise, just substitution u for t.
        varBind u t
            | u `S.member` ftv t = throwE $ "Failed the occurs check: '" ++ polyName u ++ "' with '" ++ show t ++ "' (possibly infinite type)."
            | otherwise = pure $ M.singleton u t

