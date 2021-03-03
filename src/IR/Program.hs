module IR.Program (
    Program(..)
  , progAddFunc
  -- , allocateRegisters
) where

import qualified DataStructs.HashMap as M
import IR.Instruction (VarID)
import IR.Function

-- Intermediate representation Program model
-- A program here is just represented as a set of functions
-- The actual representation uses a map from function names
-- to the functions themselves
-- The entire program is again parametric in the register model
newtype Program regModel = Program
    { funcs :: M.HashMap FuncID (Function regModel)
    }

-- Show instance for pretting printing entire programs
instance Show r => Show (Program r) where
    show (Program fs) = concatMap show (M.elems fs)

-- Functor proof for programs
instance Functor Program where
    fmap f (Program fs) = Program ((f <$>) <$> fs)

-- Helper function for adding a new function to the program
progAddFunc :: Function r -> Program r -> Program r
progAddFunc f@(Function name _ _) (Program fs) = Program (M.insert name f fs)

-- Register allocation is now just a specialisation of fmap
-- due to abstracting over the register model
-- allocateRegisters :: (VarID -> a) -> Program VarID -> Program a
-- allocateRegisters = fmap

