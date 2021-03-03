{-# LANGUAGE DeriveGeneric #-}

module SyntaxTree (
    Identifier
  , Symbol(..)
  , Literal(..)
  , Expr(..)
  , Pattern(..)
  , convertToAST

  , module Builtin
) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import qualified Parser as P
import Builtin

-- Alias for an identifier
type Identifier = String

-- A symbol can either be an identifier string or 
-- a builtin function
data Symbol
    = Identifier Identifier
    | Builtin Builtin
    deriving (Ord, Eq, Generic)

instance Hashable Symbol

-- Literals are either integers or booleans
data Literal
    = UnitLit
    | IntLit Int
    | BoolLit Bool

-- Basic expressions are one of the following.
-- Note that we in fact remove binary operations here
-- before type checking, and reconstruct them before compiling
-- The simpler the expression grammar before typing, the 
-- easier type checking is - higher level constructs can
-- be reconstructed later
data Expr t
    = App t (Expr t) (Expr t)
    | Lam t Pattern (Expr t)
    | Let t Pattern (Expr t) (Expr t)
    | IfThenElse (Expr t) (Expr t) (Expr t)
    | Lit Literal
    | Pair (Expr t) (Expr t)
    | Var t Symbol

data Pattern
    = PVar Identifier
    | PPair Pattern Pattern
    | PLit Literal

-- Helper function for determining if an expression is
-- atomic (for use in printing)
-- Atomic expressions are considered to be variables
-- and literals
isAtomic :: Expr t -> Bool
isAtomic (Lit _) = True
isAtomic (Var _ _) = True
isAtomic _ = False

-- Helper function for determining if an expression is
-- an application
isApp :: Expr t -> Bool
isApp (App _ _ _) = True
isApp _ = False

-- Show instance for symbols
instance Show Symbol where
    show (Identifier i) = i
    show (Builtin b) = show b

-- Show instance for literals
instance Show Literal where
    show UnitLit = "()"
    show (IntLit i) = show i
    show (BoolLit b) = show b

-- Show instance for expressions
instance Show t => Show (Expr t) where
    -- Applications are left associative, so if e1 is
    -- an application, we do not need to encapsulate it in 
    -- brackets
    show (App _ e1 e2) = e1s ++ " " ++ e2s
        where
            e1s
                | isApp e1 || isAtomic e1 = show e1
                | otherwise = "(" ++ show e1 ++ ")"
            e2s
                | isAtomic e2 = show e2
                | otherwise = "(" ++ show e2 ++ ")"
    -- The other terms are as expected
    show (Lam t pat e) = "$(" ++ show pat ++ ": " ++ show t ++ ") -> " ++ show e
    show (Let t pat e1 e2) = "let (" ++ show pat ++ ": " ++ show t ++ ") = " ++ show e1 ++ " in " ++ show e2
    show (IfThenElse p c a) = "if " ++ show p ++ " then " ++ show c ++ " else " ++ show a
    show (Lit l) = show l
    show (Pair l r) = "(" ++ show l ++ ", " ++ show r ++ ")"
    show (Var _ n) = show n

instance Show Pattern where
    show (PVar name) = name
    show (PPair l r) = "(" ++ show l ++ ", " ++ show r ++ ")"
    show (PLit l) = show l

-- Convert the parse tree from the parser into a cleaner, flatter AST which more accurately
-- describes the syntax, without the added clutter of the nesting precedence hierarchy etc
convertToAST :: P.Exp -> Expr ()
convertToAST = cvtExp
    where
        -- The AST from the parser has a lot of unnecessary depth
        -- to reduce parser ambiguity - here, we want to flatten
        -- the syntax into the simpler tree

        -- This process is actually overall very simple

        cvtExp :: P.Exp -> Expr ()
        cvtExp (P.Let pat body use)             = Let () (cvtPat pat) (cvtExp body) (cvtExp use)
        cvtExp (P.Application f a)              = App () (cvtExp f) (cvtExp1 a)
        cvtExp (P.Exp1 e)                       = cvtExp1 e

        cvtExp1 :: P.Exp1 -> Expr ()
        cvtExp1 (P.IfThenElse pred cons alt)    = IfThenElse (cvtExp pred) (cvtExp cons) (cvtExp alt)
        cvtExp1 (P.Lambda pat body)             = Lam () (cvtPat pat) (cvtExp body)
        cvtExp1 (P.ArithExp e)                  = cvtAExp e

        cvtAExp :: P.AExp -> Expr ()
        cvtAExp (P.Plus l r)                    = mkABinOp Add l r
        cvtAExp (P.Minus l r)                   = mkABinOp Sub l r
        cvtAExp (P.LessThan l r)                = mkCmpOp LT_ l r
        cvtAExp (P.GreaterThan l r)             = mkCmpOp GT_ l r
        cvtAExp (P.LessEqual l r)               = mkCmpOp LE_ l r
        cvtAExp (P.GreaterEqual l r)            = mkCmpOp GE_ l r
        cvtAExp (P.EqualTo l r)                 = mkCmpOp EQ_ l r
        cvtAExp (P.Term t) = cvtTerm t

        -- Slightly more intricate - we convert arithmetic operators in two
        -- levels of applications
        mkABinOp :: BinOp -> P.AExp -> P.Term -> Expr ()
        mkABinOp op l r = App () (App () (Var () $ Builtin $ BinOp op) (cvtAExp l)) (cvtTerm r)

        -- The same for comparative operators, just with the CmpOp tag
        mkCmpOp :: CmpOp -> P.AExp -> P.Term -> Expr ()
        mkCmpOp op l r = App () (App () (Var () $ Builtin $ CmpOp op) (cvtAExp l)) (cvtTerm r)

        cvtTerm :: P.Term -> Expr ()
        cvtTerm (P.Times l r)                   = mkTBinOp Mul l r
        cvtTerm (P.Div l r)                     = mkTBinOp Div l r
        cvtTerm (P.Factor f)                    = cvtFac f

        -- Because the parse tree has different layers of nesting, we need
        -- to convert multiplication and division separately - though this
        -- is essentially equivalent
        mkTBinOp :: BinOp -> P.Term -> P.Factor -> Expr ()
        mkTBinOp op l r = App () (App () (Var () $ Builtin $ BinOp op) (cvtTerm l)) (cvtFac r)

        cvtFac :: P.Factor -> Expr ()
        cvtFac (P.Unit)                         = Lit $ UnitLit
        cvtFac (P.Int i)                        = Lit $ IntLit i
        cvtFac (P.Bool b)                       = Lit $ BoolLit b
        cvtFac (P.Var v)                        = Var () $ Identifier v
        cvtFac (P.Pair l r)                     = Pair (cvtExp l) (cvtExp r)
        cvtFac (P.Nested e)                     = cvtExp e

        cvtPat :: P.Pat -> Pattern
        cvtPat (P.PVar var)                     = PVar var
        cvtPat (P.PPair l r)                    = PPair (cvtPat l) (cvtPat r)
        cvtPat (P.PUnit)                        = PLit $ UnitLit
        cvtPat (P.PInt i)                       = PLit $ IntLit i
        cvtPat (P.PBool b)                      = PLit $ BoolLit b
        cvtPat (P.PNest p)                      = cvtPat p


