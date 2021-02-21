module SyntaxTree where

import qualified Parser as P

type Identifier = String

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    deriving Show

data CmpOp
    = EQ_
    | LT_
    | GT_
    | LE_
    | GE_
    deriving Show

data Expr
    = Application Expr Expr
    | Lambda Identifier Expr
    | LetBinding Identifier Expr Expr
    | IfThenElse Expr Expr Expr
    | BinOp BinOp Expr Expr
    | CmpOp CmpOp Expr Expr
    | IntLit Int
    | BoolLit Bool
    | Var Identifier
    deriving Show

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
        cvtAExp (P.Plus l r)                 = BinOp Add (cvtAExp l) (cvtTerm r)
        cvtAExp (P.Minus l r)                = BinOp Sub (cvtAExp l) (cvtTerm r)
        cvtAExp (P.LessThan l r)             = CmpOp LT_ (cvtAExp l) (cvtTerm r)
        cvtAExp (P.GreaterThan l r)          = CmpOp GT_ (cvtAExp l) (cvtTerm r)
        cvtAExp (P.LessEqual l r)            = CmpOp LE_ (cvtAExp l) (cvtTerm r)
        cvtAExp (P.GreaterEqual l r)         = CmpOp GE_ (cvtAExp l) (cvtTerm r)
        cvtAExp (P.EqualTo l r)              = CmpOp EQ_ (cvtAExp l) (cvtTerm r)
        cvtAExp (P.Term t) = cvtTerm t

        cvtTerm :: P.Term -> Expr
        cvtTerm (P.Times l r)                = BinOp Mul (cvtTerm l) (cvtFac r)
        cvtTerm (P.Div l r)                  = BinOp Div (cvtTerm l) (cvtFac r)
        cvtTerm (P.Factor f)                 = cvtFac f

        cvtFac :: P.Factor -> Expr
        cvtFac (P.Int i)                     = IntLit i
        cvtFac (P.Bool b)                    = BoolLit b
        cvtFac (P.Var v)                     = Var v
        cvtFac (P.Nested e)                  = cvtExp e

