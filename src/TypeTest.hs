import Control.Monad.State
import Control.Monad.Trans.ExceptT
import qualified Data.Map as M
import Data.Maybe (fromJust)

data Exp
    = App Exp Exp
    | Lam String Exp
    | Let String Exp Exp
    | Add Exp Exp
    | Lit Int
    | Var String
    deriving Show

data Type
    = PolyT Int
    | IntT
    | FuncT Type Type
    deriving Show

data TExp
    = TApp TExp TExp
    | TLam Type String TExp
    | TLet String TExp TExp
    | TAdd TExp TExp
    | TLit Int
    | TVar Type String
    deriving Show

type S = (M.Map String Int, Int)

typeOf :: TExp -> TypeTree
typeOf (TApp f a) = Reduction (typeOf f) (typeOf a)
typeOf (TLam t _ b) = FuncT t (typeOf b)
typeOf (TAdd _ _) = Int
typeOf (TLit _) = Int
typeOf (TVar t _) = t

infer :: Exp -> ExceptT String Identity TExp
infer (App f a) =
    TApp (infer f) (infer a)
infer (Lam name body) =
