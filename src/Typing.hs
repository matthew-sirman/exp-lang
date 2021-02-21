module Typing where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except
import SyntaxTree

type TypeError = String

type PolyID = Int

data Type
    = IntT
    | BoolT
    | FuncT Type Type
    | Poly PolyID
    deriving Eq

data TExpr
    = TApp TExpr TExpr
    | TLambda Type Identifier TExpr
    | TLet Identifier TExpr TExpr
    | TIfThenElse TExpr TExpr TExpr
    | TBinOp BinOp TExpr TExpr
    | TCmpOp CmpOp TExpr TExpr
    | TIntLit Int
    | TBoolLit Bool
    | TVar Type Identifier
    deriving Show

polyNames :: [String]
polyNames = map (\c -> [c]) ['a'..'z'] ++ map (++ "") polyNames

instance Show Type where
    show IntT = "int"
    show BoolT = "bool"
    show (FuncT from to) = fromStr from ++ " -> " ++ show to
        where
            fromStr :: Type -> String
            fromStr f@(FuncT _ _) = "(" ++ show f ++ ")"
            fromStr other = show other
    show Poly i = polyNames !! i

typeOf :: TExpr -> Type
typeOf (TApp f _) = let (FuncT _ t) = typeOf f in t
typeOf (TLambda t _ _) = t
typeOf (TLet _ _ use) = typeOf use
typeOf (TIfThenElse _ cons _) = typeOf cons
typeOf (TBinOp _ _ _) = IntT
typeOf (TCmpOp _ _ _) = BoolT
typeOf (TIntLit _) = IntT
typeOf (TBoolLit _) = BoolT
typeOf (TVar t _) = t

type InferState = (M.Map Identifier Type, PolyID)

subType :: Type -> Type -> Bool
subType _ Poly = True
subType IntT IntT = True
subType BoolT BoolT = True
subType (FuncT f1 t1) (FuncT f2 t2) = subType f2 f1 && subType t1 t2
subType _ _ = False

findFVs :: TExpr -> S.Set Identifier
findFVs (TApp f a) = S.union (findFVs f) (findFVs a)
findFVs (TLambda _ name body) = S.delete name (findFVs body)
findFVs (TLet name body use) = S.delete name $ S.union (findFVs body) (findFVs use)
findFVs (TIfThenElse pred cons alt) = S.unions [findFVs pred, findFVs cons, findFVs alt]
findFVs (TBinOp _ lhs rhs) = S.union (findFVs lhs) (findFVs rhs)
findFVs (TCmpOp _ lhs rhs) = S.union (findFVs lhs) (findFVs rhs)
findFVs (TIntLit _) = S.empty
findFVs (TBoolLit _) = S.empty
findFVs (TVar _ name) = S.singleton name

inferTypeTree :: Expr -> (TExpr, InferState)
inferTypeTree expr = runState (itt expr Nothing) emptyState
    where
        itt :: Expr -> Maybe Type -> State InferState TExpr

        itt (Application f a) expected = do
            p <- newPoly
            typedA <- itt a $ Poly p
            typedF <- itt f $ (FuncT (typeOf typedA)) <$> expected
            pure $ TApp typedF typedA

        itt (Lambda name body) expected = do
            shadowed <- gets $ M.lookup name
            p <- newPoly
            varInsert name $ Poly p
            typedBody <- itt body to
            varType <- varLookup name
            -- Reset the shadowed variable
            case shadowed of
                Nothing -> varErase name
                Just t -> varInsert name t
            pure $ TLambda (FuncT varType (typeOf typedBody)) name typedBody

            where
                to = case expected of
                    FuncT _ t -> t
                    _ -> Poly

        itt (LetBinding name body use) expected = do
            typedBody <- itt body Poly
            varInsert name (typeOf typedBody) -- always shadow this variable
            typedUse <- itt use expected
            pure $ TLet name typedBody typedUse
            
        itt (IfThenElse pred cons alt) expected = do
            typedPred <- itt pred BoolT
            typedCons <- itt cons expected
            typedAlt <- itt alt (typeOf typedCons)
            pure $ TIfThenElse typedPred typedCons typedAlt

        itt (BinOp op lhs rhs) expected = do
            typedLhs <- itt lhs IntT
            typedRhs <- itt rhs IntT
            pure $ TBinOp op typedLhs typedRhs

        itt (CmpOp op lhs rhs) expected = do
            typedLhs <- itt lhs IntT
            typedRhs <- itt rhs IntT
            pure $ TCmpOp op typedLhs typedRhs

        itt (IntLit i) _ = pure $ TIntLit i

        itt (BoolLit b) _ = pure $ TBoolLit b

        itt (Var name) expected = do
            t <- getOrUpdate
            pure $ TVar t name

            where
                getOrUpdate = do
                    current <- varLookup name
                    if subType expected current then do
                        varInsert name expected
                        pure expected
                    else
                        pure current

        emptyState :: InferState
        emptyState = M.empty

        varLookup :: Identifier -> State InferState Type
        varLookup name = do
            t <- gets $ M.lookup name
            pure $ case t of
                    Nothing -> Poly
                    Just t' -> t'

        varInsert :: Identifier -> Type -> State InferState ()
        varInsert name t = do
            m <- get
            put $ M.insert name t m

        varErase :: Identifier -> State InferState ()
        varErase name = do
            m <- get
            put $ M.delete name m

        newPoly :: State InferState PolyID
        newPoly = do
            (pid, is) <- gets newP
            put is
            pure pid
            where
                newP (m, p) = (p, (m, p + 1))

typeCheck :: (TExpr, InferState) -> Either TypeError ()
typeCheck = runExcept . check
    where
        check :: (TExpr, InferState) -> Except TypeError ()

        check (TApp f a, vars) = do
            check (f, vars)
            check (a, vars)
            if matches then
                pure ()
            else
                throwError $ "Application of function of type '" ++ show (typeOf f) ++ "' to argument of type '"
                    ++ show (typeOf a) ++ "'"
            where
                matches =
                    case typeOf f of
                        (FuncT from _)
                            | subType (typeOf a) from -> True
                            | otherwise -> False
                        _ -> False

        check (TLambda t name body, vars) = do
            argT <- argType
            check (body, M.insert name argT vars)
            checkType

            where
                argType =
                    case t of
                        FuncT arg _ -> pure arg
                        _ -> throwError $ "Lambda must have function type."

                checkType =
                    case t of
                        FuncT _ to
                            | to == typeOf body -> pure ()
                            | otherwise -> throwError $ "Lambda type inconsistent with body type."
                        _ -> throwError $ "Lambda must have function type."

        check (TLet name body use, vars) = do
            check (body, vars)
            check (use, vars)

        check (TIfThenElse pred cons alt, vars) = do
            check (pred, vars)
            checkPred
            check (cons, vars)
            check (alt, vars)
            checkBranches

            where
                checkPred =
                    case typeOf pred of
                        BoolT -> pure ()
                        t -> throwError $ "Predicate in 'if' statement must have type 'bool'; found '" ++ show t ++ "'."

                checkBranches =
                    let tCons = typeOf cons
                        tAlt = typeOf alt in
                    if tCons == tAlt then
                        pure ()
                    else
                        throwError $ "Branches in 'if' statement must be of the same type: found '" ++ show tCons 
                            ++ "' and '" ++ show tAlt ++ "'."

        check (TBinOp _ lhs rhs, vars) = do
            check (lhs, vars)
            checkSide lhs
            check (rhs, vars)
            checkSide rhs

            where
                checkSide :: TExpr -> Except TypeError ()
                checkSide s =
                    case typeOf s of
                        IntT -> pure ()
                        t -> throwError $ "Arguments of integer operator must be of type 'int'."

        check (TCmpOp _ lhs rhs, vars) = do
            check (lhs, vars)
            checkSide lhs
            check (rhs, vars)
            checkSide rhs

            where
                checkSide :: TExpr -> Except TypeError ()
                checkSide s =
                    case typeOf s of
                        IntT -> pure ()
                        t -> throwError $ "Arguments of comparison operator must be of type 'int'."

        check (TIntLit _, _) = pure ()

        check (TBoolLit _, _) = pure ()

        check (TVar t name, vars) =
            case M.lookup name vars of
                Just t'
                    | subType t' t -> pure ()
                    | otherwise -> throwError $ "Inconsistent typing for variable '" ++ name 
                        ++ "'. (found both '" ++ show t ++ "' and '" ++ show t' ++ "')."


