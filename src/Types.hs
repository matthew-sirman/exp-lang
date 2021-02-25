module Types where

-- Polymorphic type disambiguation
type PolyID = Int

-- Types are built from this simple grammar
data Type
    = IntTy
    | BoolTy
    | FuncTy Type Type
    | PolyTy PolyID
    
-- Helper function to return if a type is a boolean
isBoolTy :: Type -> Bool
isBoolTy BoolTy = True
isBoolTy _ = False

-- Helper function to return if a type is an int
isIntTy :: Type -> Bool
isIntTy IntTy = True
isIntTy _ = False

-- Infinite list of type names for pretty printing. These
-- will enumerate the lowercase alphabet, at then again with a prime etc
-- e.g.
--  a, b, ..., y, z, a', b', ..., y', z', a''...
-- This gives an easy mapping for printing polymorphic type variables
polyNames :: [String]
polyNames = map (\c -> [c]) ['a'..'z'] ++ map (++ "'") polyNames

-- Helper function for mapping a polymorphic type ID to
-- a name - just index the infinite list above
polyName :: PolyID -> String
polyName = (polyNames !!)

-- Show instance for pretting printing types
instance Show Type where
    show IntTy = "int"
    show BoolTy = "bool"
    -- Function types are right associative, so if the left hand
    -- (argument) of a function type is in itself a function, we
    -- bracket it. The right hand side is always unbracketed
    show (FuncTy from to) = fromStr from ++ " -> " ++ show to
        where
            fromStr :: Type -> String
            fromStr f@(FuncTy _ _) = "(" ++ show f ++ ")"
            fromStr other = show other
    show (PolyTy i) = polyName i
