{-# LANGUAGE ScopedTypeVariables #-}

module IR where

import qualified Data.Map as M
import Data.Sequence as Seq
import Control.Monad.Except
import Builtin

type Label = String
type VarID = Int
type FuncID = String
type PhiNode r = (Value r, Label)

showLabel :: Label -> String
showLabel l = l

showVar :: Show v => v -> String
showVar v = "tmp" ++ show v

showFunc :: FuncID -> String
showFunc f = f

showPhiNode :: Show r => PhiNode r -> String
showPhiNode (v, lab) = show v ++ " : " ++ showLabel lab

data Immediate
    = Int64 Int
    | Bool Bool
    deriving (Ord, Eq)

instance Show Immediate where
    show (Int64 i) = "i64 " ++ show i
    show (Bool b) = "i1 " ++ bin b
        where
            bin True = "1"
            bin False = "0"

data Value r
    = Immediate Immediate
    | Variable r
    | UserFunc FuncID
    | Builtin Builtin
    | Argument
    deriving (Ord, Eq)

instance Show r => Show (Value r) where
    show (Immediate imm) = show imm
    show (Variable v) = showVar v
    show (UserFunc f) = showFunc f
    show (Builtin b) = show b
    show Argument = "arg"

instance Functor Value where
    fmap _ (Immediate imm) = Immediate imm
    fmap f (Variable v) = Variable (f v)
    fmap _ (UserFunc f) = UserFunc f
    fmap _ (Builtin b) = Builtin b
    fmap _ Argument = Argument

data Instruction r
    = Add r (Value r) (Value r)         -- adds two values and assigns to VarID
    | Sub r (Value r) (Value r)         -- subtracts two values and assigns to VarID
    | Mul r (Value r) (Value r)         -- multiplies two values and assigns to VarID
    | Div r (Value r) (Value r)         -- divids two values and assigns to VarID
    | EQ r (Value r) (Value r)          -- tests whether lhs = rhs and assigns to VarID
    | LT r (Value r) (Value r)          -- tests whether lhs < rhs and assigsn to VarID
    | GT r (Value r) (Value r)          -- tests whether lhs > rhs and assigsn to VarID
    | LE r (Value r) (Value r)          -- tests whether lhs <= rhs and assigsn to VarID
    | GE r (Value r) (Value r)          -- tests whether lhs >= rhs and assigsn to VarID
    | Call r (Value r) (Value r)        -- calls a function with a value, and assigns to VarID
    | Branch (Value r) Label            -- conditional branch to label on value
    | Jump Label                        -- unconditional branch to label
    | Phi r (PhiNode r) (PhiNode r)     -- coalesces two values into a VarID based on branches
    | Ret (Value r)                     -- returns a value from a function
    deriving (Ord, Eq)

instance Show r => Show (Instruction r) where
    show (IR.Add v e1 e2) = showVar v ++ " = add " ++ show e1 ++ ", " ++ show e2 
    show (IR.Sub v e1 e2) = showVar v ++ " = sub " ++ show e1 ++ ", " ++ show e2 
    show (IR.Mul v e1 e2) = showVar v ++ " = mul " ++ show e1 ++ ", " ++ show e2 
    show (IR.Div v e1 e2) = showVar v ++ " = div " ++ show e1 ++ ", " ++ show e2 

    show (IR.EQ v e1 e2) = showVar v ++ " = eq " ++ show e1 ++ ", " ++ show e2
    show (IR.LT v e1 e2) = showVar v ++ " = lt " ++ show e1 ++ ", " ++ show e2
    show (IR.GT v e1 e2) = showVar v ++ " = gt " ++ show e1 ++ ", " ++ show e2
    show (IR.LE v e1 e2) = showVar v ++ " = le " ++ show e1 ++ ", " ++ show e2
    show (IR.GE v e1 e2) = showVar v ++ " = ge " ++ show e1 ++ ", " ++ show e2

    show (Call v f a) = showVar v ++ " = call " ++ show f ++ "(" ++ show a ++ ")"
    show (Branch v lab) = "br " ++ show v ++ ", " ++ showLabel lab
    show (Jump lab) = "br " ++ showLabel lab

    show (Phi v p1 p2) = showVar v ++ " = phi [" ++ showPhiNode p1 ++ ", " ++ showPhiNode p2 ++ "]"
    show (Ret v) = "ret " ++ show v

instance Functor Instruction where
    fmap f (IR.Add v l r)   = IR.Add (f v) (f <$> l) (f <$> r)
    fmap f (IR.Sub v l r)   = IR.Sub (f v) (f <$> l) (f <$> r)
    fmap f (IR.Mul v l r)   = IR.Mul (f v) (f <$> l) (f <$> r)
    fmap f (IR.Div v l r)   = IR.Div (f v) (f <$> l) (f <$> r)
    fmap f (IR.EQ v l r)    = IR.EQ (f v) (f <$> l) (f <$> r)
    fmap f (IR.LT v l r)    = IR.LT (f v) (f <$> l) (f <$> r)
    fmap f (IR.GT v l r)    = IR.GT (f v) (f <$> l) (f <$> r)
    fmap f (IR.LE v l r)    = IR.LE (f v) (f <$> l) (f <$> r)
    fmap f (IR.GE v l r)    = IR.GE (f v) (f <$> l) (f <$> r)
    fmap f (IR.Call v cf a) = IR.Call (f v) (f <$> cf) (f <$> a)
    fmap f (IR.Branch v l)  = IR.Branch (f <$> v) l
    fmap f (IR.Jump l)      = IR.Jump l
    fmap f (IR.Phi v (vl, ll) (vr, lr)) = IR.Phi (f v) (f <$> vl, ll) (f <$> vr, lr)
    fmap f (IR.Ret v)       = IR.Ret (f <$> v)
   
data BasicBlock r = BasicBlock
    { label :: Label
    , iList :: Seq (Instruction r)
    }

instance Show r => Show (BasicBlock r) where
    show (BasicBlock lab is) = showLabel lab ++ ":\n" ++ concatMap (\i -> "    " ++ show i ++ "\n") is

instance Functor BasicBlock where
    fmap f bb@(BasicBlock _ is) = bb { iList = (f <$>) <$> is }

mkBasicBlock :: Label -> BasicBlock r
mkBasicBlock lab = BasicBlock lab Seq.empty

blockIPushFront :: Instruction r -> BasicBlock r -> BasicBlock r
blockIPushFront i blk = blk { iList = i :<| iList blk }

blockIPush :: Instruction r -> BasicBlock r -> BasicBlock r
blockIPush i blk = blk { iList = iList blk :|> i }

data Function r = Function
    { fid :: FuncID
    , blocks :: Seq (BasicBlock r)
    }

instance Show r => Show (Function r) where
    show (Function f bs) = showFunc f ++ "(" ++ show (Argument :: Value r) ++ ") {\n" ++ concatMap show bs ++ "}\n\n"

instance Functor Function where
    fmap f (Function name bs) = Function name ((f <$>) <$> bs)

mkFunc :: FuncID -> Function r
mkFunc nm = Function nm Seq.empty

pushBlock :: BasicBlock r -> Function r -> Function r
pushBlock blk (Function name bs) = Function name (bs :|> blk)

newtype Program regModel = Program
    { funcs :: M.Map FuncID (Function regModel)
    }

instance Show r => Show (Program r) where
    show (Program fs) = concatMap show (M.elems fs)

instance Functor Program where
    fmap f (Program fs) = Program ((f <$>) <$> fs)

progAddFunc :: Function r -> Program r -> Program r
progAddFunc f@(Function name _) (Program fs) = Program (M.insert name f fs)

allocateRegisters :: (VarID -> a) -> Program VarID -> Program a
allocateRegisters = fmap

