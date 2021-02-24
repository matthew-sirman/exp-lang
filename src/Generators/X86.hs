module Generators.X86 where

import Data.Sequence as Seq
import qualified IR

type Label = String

data X86Register
    = RAX
    | RBX
    | RCX
    | RDX

data X86Instruction
    = Add X86Register X86Register
    | Sub X86Register X86Register

data X86Region = X86Region
    { label :: Label
    , instructions :: Seq X86Instruction
    }

data X86TextSection = X86TextSection
    { globals :: [Label]
    }

data X86Code = X86Code
    { textSection :: X86TextSection
    , regions :: Seq X86Region
    }

generateX86 :: IR.Program -> X86Code
generateX86 (IR.Program functions) = undefined

