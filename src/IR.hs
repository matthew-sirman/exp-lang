module IR where

import qualified Data.Map as M
import Data.Sequence as Seq
import Builtin

type Label = String
type VarID = Int
type FuncID = String
type PhiNode = (Value, Label)

showLabel :: Label -> String
showLabel l = l

showVar :: VarID -> String
showVar v = "tmp" ++ show v

showFunc :: FuncID -> String
showFunc f = f

showPhiNode :: PhiNode -> String
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

data Value
    = Immediate Immediate
    | Variable VarID
    | UserFunc FuncID
    | Builtin Builtin
    | Argument
    deriving (Ord, Eq)

instance Show Value where
    show (Immediate imm) = show imm
    show (Variable v) = showVar v
    show (UserFunc f) = showFunc f
    show (Builtin b) = show b
    show Argument = "arg"

data Instruction
    = Add VarID Value Value         -- adds two values and assigns to VarID
    | Sub VarID Value Value         -- subtracts two values and assigns to VarID
    | Mul VarID Value Value         -- multiplies two values and assigns to VarID
    | Div VarID Value Value         -- divids two values and assigns to VarID
    | EQ VarID Value Value          -- tests whether lhs = rhs and assigns to VarID
    | LT VarID Value Value          -- tests whether lhs < rhs and assigsn to VarID
    | GT VarID Value Value          -- tests whether lhs > rhs and assigsn to VarID
    | LE VarID Value Value          -- tests whether lhs <= rhs and assigsn to VarID
    | GE VarID Value Value          -- tests whether lhs >= rhs and assigsn to VarID
    | Call VarID Value Value        -- calls a function with a value, and assigns to VarID
    | Branch Value Label            -- conditional branch to label on value
    | Jump Label                    -- unconditional branch to label
    | Phi VarID PhiNode PhiNode     -- coalesces two values into a VarID based on branches
    | Ret Value                     -- returns a value from a function
    deriving (Ord, Eq)

instance Show Instruction where
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
    
data BasicBlock = BasicBlock
    { label :: Label
    , iList :: Seq Instruction
    }

instance Show BasicBlock where
    show (BasicBlock lab is) = showLabel lab ++ ":\n" ++ concatMap (\i -> "    " ++ show i ++ "\n") is

mkBasicBlock :: Label -> BasicBlock
mkBasicBlock lab = BasicBlock lab Seq.empty

blockIPushFront :: Instruction -> BasicBlock -> BasicBlock
blockIPushFront i blk = blk { iList = i :<| iList blk }

blockIPush :: Instruction -> BasicBlock -> BasicBlock
blockIPush i blk = blk { iList = iList blk :|> i }

data Function = Function
    { fid :: FuncID
    , blocks :: Seq BasicBlock
    }

instance Show Function where
    show (Function f bs) = showFunc f ++ "(" ++ show Argument ++ ") {\n" ++ concatMap show bs ++ "}\n\n"

mkFunc :: FuncID -> Function
mkFunc nm = Function nm Seq.empty

pushBlock :: BasicBlock -> Function -> Function
pushBlock blk func = func { blocks = blocks func :|> blk }

newtype Program = Program
    { funcs :: M.Map FuncID Function
    }

instance Show Program where
    show (Program fs) = concatMap show (M.elems fs)

progAddFunc :: Function -> Program -> Program
progAddFunc f@(Function name _) prog = prog { funcs = M.insert name f (funcs prog) }

