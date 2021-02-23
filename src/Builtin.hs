module Builtin where

import Types

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    deriving (Ord, Eq)

data CmpOp
    = EQ_
    | LT_
    | GT_
    | LE_
    | GE_
    deriving (Ord, Eq)

data Builtin
    = BinOp BinOp
    | CmpOp CmpOp
    deriving (Ord, Eq)

instance Show BinOp where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

instance Show CmpOp where
    show EQ_ = "=="
    show LT_ = "<"
    show GT_ = ">"
    show LE_ = "<="
    show GE_ = ">="

instance Show Builtin where
    show (BinOp b) = show b
    show (CmpOp c) = show c

builtinType :: Builtin -> Type
builtinType (BinOp _) = FuncTy IntTy (FuncTy IntTy IntTy)
builtinType (CmpOp _) = FuncTy IntTy (FuncTy IntTy BoolTy)

builtinList :: [Builtin]
builtinList = bs ++ cs
    where
        bs = map BinOp [Add, Sub, Mul, Div]
        cs = map CmpOp [EQ_, LT_, GT_, LE_, GE_]

