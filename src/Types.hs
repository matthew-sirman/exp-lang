module Types where

type PolyID = Int

data Type
    = IntTy
    | BoolTy
    | FuncTy Type Type
    | PolyTy PolyID
    
isBoolTy :: Type -> Bool
isBoolTy BoolTy = True
isBoolTy _ = False

isIntTy :: Type -> Bool
isIntTy IntTy = True
isIntTy _ = False

polyNames :: [String]
polyNames = map (\c -> [c]) ['a'..'z'] ++ map (++ "") polyNames

polyName :: PolyID -> String
polyName = (polyNames !!)

instance Show Type where
    show IntTy = "int"
    show BoolTy = "bool"
    show (FuncTy from to) = fromStr from ++ " -> " ++ show to
        where
            fromStr :: Type -> String
            fromStr f@(FuncTy _ _) = "(" ++ show f ++ ")"
            fromStr other = show other
    show (PolyTy i) = polyNames !! i

