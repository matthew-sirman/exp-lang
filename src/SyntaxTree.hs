module SyntaxTree where

import qualified Parser as P
import Builtin

type Identifier = String

data Symbol
    = Identifier Identifier
    | Builtin Builtin
    deriving (Ord, Eq)

data Literal
    = IntLit Int
    | BoolLit Bool

data Expr
    = Application Expr Expr
    | Lambda Identifier Expr
    | LetBinding Identifier Expr Expr
    | IfThenElse Expr Expr Expr
    | Lit Literal
    | Var Symbol

isAtomic :: Expr -> Bool
isAtomic (Lit _) = True
isAtomic (Var _) = True
isAtomic _ = False

isApp :: Expr -> Bool
isApp (Application _ _) = True
isApp _ = False

instance Show Symbol where
    show (Identifier i) = i
    show (Builtin b) = show b

instance Show Literal where
    show (IntLit i) = show i
    show (BoolLit b) = show b

instance Show Expr where
    show (Application e1 e2) = e1s ++ " " ++ e2s
        where
            e1s
                | isApp e1 || isAtomic e1 = show e1
                | otherwise = "(" ++ show e1 ++ ")"
            e2s
                | isAtomic e2 = show e2
                | otherwise = "(" ++ show e2 ++ ")"
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

        mkABinOp :: BinOp -> P.AExp -> P.Term -> Expr
        mkABinOp op l r = Application (Application (Var $ Builtin $ BinOp op) (cvtAExp l)) (cvtTerm r)

        mkCmpOp :: CmpOp -> P.AExp -> P.Term -> Expr
        mkCmpOp op l r = Application (Application (Var $ Builtin $ CmpOp op) (cvtAExp l)) (cvtTerm r)

        cvtTerm :: P.Term -> Expr
        cvtTerm (P.Times l r)                = mkTBinOp Mul l r
        cvtTerm (P.Div l r)                  = mkTBinOp Div l r
        cvtTerm (P.Factor f)                 = cvtFac f

        mkTBinOp :: BinOp -> P.Term -> P.Factor -> Expr
        mkTBinOp op l r = Application (Application (Var $ Builtin $ BinOp op) (cvtTerm l)) (cvtFac r)

        cvtFac :: P.Factor -> Expr
        cvtFac (P.Int i)                     = Lit $ IntLit i
        cvtFac (P.Bool b)                    = Lit $ BoolLit b
        cvtFac (P.Var v)                     = Var $ Identifier v
        cvtFac (P.Nested e)                  = cvtExp e

