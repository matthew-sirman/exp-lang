module X86 (
    X86Register(..)
  , X86Inst(..)
  , X86Region(..)
  , X86TextSection(..)
  , X86Code(..)
  , optimiseX86
  , generateX86
) where

import Compiler

data X86Register
    = RAX
    | RBX
    | RCX
    | RDX
    deriving (Show, Eq)

data X86Inst
    = X86Add X86Register X86Register
    | X86Sub X86Register X86Register
    | X86Push X86Register
    | X86Pop X86Register
    | X86Mov X86Register Int
    | X86Ret
    deriving Show

data X86Region = X86Region
    { label :: String
    , insts :: [X86Inst]
    }
    deriving Show

data X86TextSection = X86TextSection
    { globals :: [X86Region]
    }
    deriving Show

data X86Code = X86Code
    { textSection :: X86TextSection
    , regions :: [X86Region]
    }
    deriving Show

optimiseX86 ::Int -> X86Code -> X86Code
optimiseX86 passes code = code { regions = map (doPasses passes optRegion) $ regions code }
    where
        optRegion :: X86Region -> X86Region
        optRegion reg = reg { insts = opt $ insts reg }

        opt :: [X86Inst] -> [X86Inst]
        opt [] = []
        opt (i1@(X86Push r1) : i2@(X86Pop r2) : rest)
            | r1 == r2 = opt rest   -- if we see a push then pop for the same register, just remove these
            | otherwise = i1 : i2 : opt rest
        opt (i:rest) = i : opt rest

        doPasses :: Int -> (a -> a) -> (a -> a)
        doPasses 0 _ = id
        doPasses n f = f . doPasses (n - 1) f


generateX86 :: String -> [Instruction] -> X86Code
generateX86 fName iList = X86Code ts [reg]
    where
        ts = X86TextSection [reg]
        reg = X86Region fName $ embellish $ genInstructions iList

        embellish is = [X86Push RBX] ++ is ++ [X86Pop RAX, X86Pop RBX, X86Ret]

        genInstructions [] = []
        genInstructions (i:is) = genI i ++ genInstructions is

        -- Pop top two values off the stack, add them together, and push the result back
        genI Add = [X86Pop RAX, X86Pop RBX, X86Add RAX RBX, X86Push RAX]
        genI Sub = [X86Pop RAX, X86Pop RBX, X86Sub RAX RBX, X86Push RAX]

        -- Undefined for now
        genI Mul = undefined
        genI Div = undefined

        -- Push an integer value onto the stack
        genI (Push (Int64 i)) = [X86Mov RAX i, X86Push RAX]

