module IR.Instruction (
    VarID
  , Label
  , FuncID
  , PhiNode
  , Immediate(..)
  , Value(..)
  , Instruction(..)
) where

import Prelude hiding (EQ, LT, GT)

-- Type aliases for various constructs in the IR

-- Temporary variable type - by default just an integer
-- representing an infinite set of registers
type VarID = Int
-- Label for a block segment
type Label = String
-- Function name type
type FuncID = String
-- Type of a phi coalescence node
type PhiNode r = (Value r, Label)

-- Pseudo show instances for the above types
showVar :: Show v => v -> String
showVar v = "tmp" ++ show v

showLabel :: Label -> String
showLabel l = l

showFunc :: FuncID -> String
showFunc f = f

showPhiNode :: Show r => PhiNode r -> String
showPhiNode (v, lab) = show v ++ " : " ++ showLabel lab


-- Immediate data type for values which can be used as literals
data Immediate
    = Int64 Int
    | Bool Bool
    deriving (Ord, Eq)

-- Show instance for literals - prepends the values with type 
-- representations
instance Show Immediate where
    show (Int64 i) = "i64 " ++ show i
    show (Bool b) = "i1 " ++ bin b
        where
            bin True = "1"
            bin False = "0"

-- Values in the IR. This is parametric in the register model.
-- Values in the IR can be
--  Immediate literals
--  Register variables
--  Function labels
--  Function arguments
data Value r
    = Immediate Immediate
    | Variable r
    | UserFunc FuncID
    -- | Builtin Builtin
    | Argument
    deriving (Ord, Eq)

-- Show instance for IR values to help with pretty printing
instance Show r => Show (Value r) where
    show (Immediate imm) = show imm
    show (Variable v) = showVar v
    show (UserFunc f) = showFunc f
    -- show (Builtin b) = show b
    show Argument = "arg"

-- Functor proof for values - the parametric register
-- type can be updated
instance Functor Value where
    fmap _ (Immediate imm) = Immediate imm
    fmap f (Variable v) = Variable (f v)
    fmap _ (UserFunc f) = UserFunc f
    -- fmap _ (Builtin b) = Builtin b
    fmap _ Argument = Argument

-- Instruction type in the IR. This is also parametric in the
-- register model
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

-- Show instance for the instructions for pretty printing
instance Show r => Show (Instruction r) where
    show (Add v e1 e2) = showVar v ++ " = add " ++ show e1 ++ ", " ++ show e2 
    show (Sub v e1 e2) = showVar v ++ " = sub " ++ show e1 ++ ", " ++ show e2 
    show (Mul v e1 e2) = showVar v ++ " = mul " ++ show e1 ++ ", " ++ show e2 
    show (Div v e1 e2) = showVar v ++ " = div " ++ show e1 ++ ", " ++ show e2 

    show (EQ v e1 e2) = showVar v ++ " = eq " ++ show e1 ++ ", " ++ show e2
    show (LT v e1 e2) = showVar v ++ " = lt " ++ show e1 ++ ", " ++ show e2
    show (GT v e1 e2) = showVar v ++ " = gt " ++ show e1 ++ ", " ++ show e2
    show (LE v e1 e2) = showVar v ++ " = le " ++ show e1 ++ ", " ++ show e2
    show (GE v e1 e2) = showVar v ++ " = ge " ++ show e1 ++ ", " ++ show e2

    show (Call v f a) = showVar v ++ " = call " ++ show f ++ "(" ++ show a ++ ")"
    show (Branch v lab) = "br " ++ show v ++ ", " ++ showLabel lab
    show (Jump lab) = "br " ++ showLabel lab

    show (Phi v p1 p2) = showVar v ++ " = phi [" ++ showPhiNode p1 ++ ", " ++ showPhiNode p2 ++ "]"
    show (Ret v) = "ret " ++ show v

-- Functor proof for instructions
instance Functor Instruction where
    fmap f (Add v l r)   = Add (f v) (f <$> l) (f <$> r)
    fmap f (Sub v l r)   = Sub (f v) (f <$> l) (f <$> r)
    fmap f (Mul v l r)   = Mul (f v) (f <$> l) (f <$> r)
    fmap f (Div v l r)   = Div (f v) (f <$> l) (f <$> r)
    fmap f (EQ v l r)    = EQ (f v) (f <$> l) (f <$> r)
    fmap f (LT v l r)    = LT (f v) (f <$> l) (f <$> r)
    fmap f (GT v l r)    = GT (f v) (f <$> l) (f <$> r)
    fmap f (LE v l r)    = LE (f v) (f <$> l) (f <$> r)
    fmap f (GE v l r)    = GE (f v) (f <$> l) (f <$> r)
    fmap f (Call v cf a) = Call (f v) (f <$> cf) (f <$> a)
    fmap f (Branch v l)  = Branch (f <$> v) l
    fmap f (Jump l)      = Jump l
    fmap f (Phi v (vl, ll) (vr, lr)) = Phi (f v) (f <$> vl, ll) (f <$> vr, lr)
    fmap f (Ret v)       = Ret (f <$> v)
 
