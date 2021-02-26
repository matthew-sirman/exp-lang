module Generators.X86_64 where

import Data.Sequence as Seq
import qualified IR

type Label = String

data Register
    = RAX
    | RBX
    | RCX
    | RDX
    | RSI
    | RDI
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    deriving Show

data Instruction
    = Add Register Register
    | Sub Register Register

data Region = Region
    { label :: Label
    , instructions :: Seq Instruction
    }

data TextSection = TextSection
    { globals :: [Label]
    , externs :: [Label]
    }

data X86_64Code = X86_64Code
    { textSection :: TextSection
    , regions :: Seq Region
    }

allocateRegisters :: IR.Program IR.VarID -> IR.Program Register
allocateRegisters (IR.Program fs) = IR.Program $ fAlloc <$> fs
    where
        fAlloc :: IR.Function IR.VarID -> IR.Function Register
        fAlloc = undefined

generateX86 :: IR.Program IR.VarID -> X86_64Code
generateX86 (IR.Program functions) = undefined

