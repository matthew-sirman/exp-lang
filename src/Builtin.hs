{-# LANGUAGE DeriveGeneric #-}

module Builtin (
    BinOp(..)
  , CmpOp(..)
  , Builtin(..)
  , builtinType
  , builtinList
) where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Types

-- Data tag type for an arithmetic binary operator
-- of type int -> int -> int
data BinOp
    = Add
    | Sub
    | Mul
    | Div
    deriving (Ord, Eq, Generic)

-- Data tag type for a comparative binary operator
-- of type int -> int -> bool
data CmpOp
    = EQ_
    | LT_
    | GT_
    | LE_
    | GE_
    deriving (Ord, Eq, Generic)

-- Data tag type for any builtin function
-- Includes:
--  Arithmetic binary operators
--  Comparative binary operators
data Builtin
    = BinOp BinOp
    | CmpOp CmpOp
    deriving (Ord, Eq, Generic)

-- Show instance for arithmetic binary operators
instance Show BinOp where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

instance Hashable BinOp

-- Show instance for comparative binary operators
instance Show CmpOp where
    show EQ_ = "=="
    show LT_ = "<"
    show GT_ = ">"
    show LE_ = "<="
    show GE_ = ">="

instance Hashable CmpOp

-- Show instance for any builtin
instance Show Builtin where
    show (BinOp b) = show b
    show (CmpOp c) = show c

instance Hashable Builtin

-- Return the functional type of a given binary operator
builtinType :: Builtin -> Type
builtinType (BinOp _) = FuncTy IntTy (FuncTy IntTy IntTy)
builtinType (CmpOp _) = FuncTy IntTy (FuncTy IntTy BoolTy)

-- Get a list of all the available builtins
builtinList :: [Builtin]
builtinList = bs ++ cs
    where
        bs = map BinOp [Add, Sub, Mul, Div]
        cs = map CmpOp [EQ_, LT_, GT_, LE_, GE_]

