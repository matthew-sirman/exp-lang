module SyntaxTree (
    Identifier
  , Symbol(..)
  , Literal(..)
  , Expr(..)
  , convertToAST

  , module Builtin
) where

import qualified Parser as P
import Builtin

-- Alias for an identifier
type Identifier = String

-- A symbol can either be an identifier string or 
-- a builtin function
data Symbol
    = Identifier Identifier
    | Builtin Builtin
    deriving (Ord, Eq)

-- Literals are either integers or booleans
data Literal
    = IntLit Int
    | BoolLit Bool

-- Basic expressions are one of the following.
-- Note that we in fact remove binary operations here
-- before type checking, and reconstruct them before compiling
-- The simpler the expression grammar before typing, the 
-- easier type checking is - higher level constructs can
-- be reconstructed later
data Expr
    = Application Expr Expr
    | Lambda Identifier Expr
    | LetBinding Identifier Expr Expr
    | IfThenElse Expr Expr Expr
    | Lit Literal
    | Var Symbol

-- Helper function for determining if an expression is
-- atomic (for use in printing)
-- Atomic expressions are considered to be variables
-- and literals
isAtomic :: Expr -> Bool
isAtomic (Lit _) = True
isAtomic (Var _) = True
isAtomic _ = False

-- Helper function for determining if an expression is
-- an application
isApp :: Expr -> Bool
isApp (Application _ _) = True
isApp _ = False

-- Show instance for symbols
instance Show Symbol where
    show (Identifier i) = i
    show (Builtin b) = show b

-- Show instance for literals
instance Show Literal where
    show (IntLit i) = show i
    show (BoolLit b) = show b

-- Show instance for expressions
instance Show Expr where
    -- Applications are left associative, so if e1 is
    -- an application, we do not need to encapsulate it in 
    -- brackets
    show (Application e1 e2) = e1s ++ " " ++ e2s
        where
            e1s
                | isApp e1 || isAtomic e1 = show e1
                | otherwise = "(" ++ show e1 ++ ")"
            e2s
                | isAtomic e2 = show e2
                | otherwise = "(" ++ show e2 ++ ")"
    -- The other terms are as expected
    show (Lambda name e) = "$" ++ name ++ " -> " ++ show e
    show (LetBinding name e1 e2) = "let " ++ name ++ " = " ++ show e1 ++ " in " ++ show e2
    show (IfThenElse p c a) = "if " ++ show p ++ " then " ++ show c ++ " else " ++ show a
    show (Lit l) = show l
    show (Var n) = show n

-- Convert the parse tree from the parser into a cleaner, flatter AST which more accurately
-- describes the syntax, without the added clutter of the nesting precedence hierarchy etc
convertToAST :: P.Exp -> Expr
convertToAST = cvtExp
    where
        -- The AST from the parser has a lot of unnecessary depth
        -- to reduce parser ambiguity - here, we want to flatten
        -- the syntax into the simpler tree

        -- This process is actually overall very simple

        cvtExp :: P.Exp -> Expr
        cvtExp (P.Let var body use)          = LetBinding var (cvtExp body) (cvtExp use)
        cvtExp (P.Application f a)           = Application (cvtExp f) (cvtExp1 a)
        cvtExp (P.Exp1 e)                    = cvtExp1 e

        cvtExp1 :: P.Exp1 -> Expr
        cvtExp1 (P.IfThenElse pred cons alt) = IfThenElse (cvtExp pred) (cvtExp cons) (cvtExp alt)
        cvtExp1 (P.Lambda var body)          = Lambda var (cvtExp body)
        cvtExp1 (P.ArithExp e)               = cvtAExp e

        cvtAExp :: P.AExp -> Expr
        cvtAExp (P.Plus l r)                 = mkABinOp Add l r
        cvtAExp (P.Minus l r)                = mkABinOp Sub l r
        cvtAExp (P.LessThan l r)             = mkCmpOp LT_ l r
        cvtAExp (P.GreaterThan l r)          = mkCmpOp GT_ l r
        cvtAExp (P.LessEqual l r)            = mkCmpOp LE_ l r
        cvtAExp (P.GreaterEqual l r)         = mkCmpOp GE_ l r
        cvtAExp (P.EqualTo l r)              = mkCmpOp EQ_ l r
        cvtAExp (P.Term t) = cvtTerm t

        -- Slightly more intricate - we convert arithmetic operators in two
        -- levels of applications
        mkABinOp :: BinOp -> P.AExp -> P.Term -> Expr
        mkABinOp op l r = Application (Application (Var $ Builtin $ BinOp op) (cvtAExp l)) (cvtTerm r)

        -- The same for comparative operators, just with the CmpOp tag
        mkCmpOp :: CmpOp -> P.AExp -> P.Term -> Expr
        mkCmpOp op l r = Application (Application (Var $ Builtin $ CmpOp op) (cvtAExp l)) (cvtTerm r)

        cvtTerm :: P.Term -> Expr
        cvtTerm (P.Times l r)                = mkTBinOp Mul l r
        cvtTerm (P.Div l r)                  = mkTBinOp Div l r
        cvtTerm (P.Factor f)                 = cvtFac f

        -- Because the parse tree has different layers of nesting, we need
        -- to convert multiplication and division separately - though this
        -- is essentially equivalent
        mkTBinOp :: BinOp -> P.Term -> P.Factor -> Expr
        mkTBinOp op l r = Application (Application (Var $ Builtin $ BinOp op) (cvtTerm l)) (cvtFac r)

        cvtFac :: P.Factor -> Expr
        cvtFac (P.Int i)                     = Lit $ IntLit i
        cvtFac (P.Bool b)                    = Lit $ BoolLit b
        cvtFac (P.Var v)                     = Var $ Identifier v
        cvtFac (P.Nested e)                  = cvtExp e

