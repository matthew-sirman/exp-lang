module IR where

import qualified Data.Map as M
import Data.Sequence as Seq

type Label = Int
type VarID = Int
type FuncID = Int
type PhiNode = (Value, Label)

data Immediate
    = Int64 Int
    | Bool Bool
    deriving Show

data Value
    = Immediate Immediate
    | Variable VarID
    | Func FuncID
    | Argument
    deriving Show

data Instruction
    = Add VarID Value Value            -- adds two values and assigns to VarID
    | Sub VarID Value Value            -- subtracts two values and assigns to VarID
    | Mul VarID Value Value            -- multiplies two values and assigns to VarID
    | Div VarID Value Value            -- divids two values and assigns to VarID
    | EQ VarID Value Value             -- tests whether lhs = rhs and assigns to VarID
    | LT VarID Value Value             -- tests whether lhs < rhs and assigsn to VarID
    | GT VarID Value Value             -- tests whether lhs > rhs and assigsn to VarID
    | LE VarID Value Value             -- tests whether lhs <= rhs and assigsn to VarID
    | GE VarID Value Value             -- tests whether lhs >= rhs and assigsn to VarID
    | Call VarID FuncID Value          -- calls a function with a value, and assigns to VarID
    | Branch Value Label               -- conditional branch to label on value
    | Jump Label                       -- unconditional branch to label
    | Phi VarID PhiNode PhiNode        -- coalesces two values into a VarID based on branches
    | Ret Value                        -- returns a value from a function
    deriving Show

data BasicBlock = BasicBlock
    { label :: Label
    , iList :: Seq Instruction
    }
    deriving Show

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
    deriving Show

mkFunc :: FuncID -> Function
mkFunc nm = Function nm Seq.empty

pushBlock :: BasicBlock -> Function -> Function
pushBlock blk func = func { blocks = blocks func :|> blk }

data Program = Program
    { mainBlock :: BasicBlock
    , funcs :: M.Map FuncID Function
    }
    deriving Show

progAddFunc :: Function -> Program -> Program
progAddFunc f@(Function name _) prog = prog { funcs = M.insert name f (funcs prog) }

