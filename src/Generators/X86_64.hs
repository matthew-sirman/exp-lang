{-# LANGUAGE DeriveGeneric #-}

module Generators.X86_64 where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
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
    deriving (Ord, Eq, Show, Generic)

instance Hashable Register

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

data X86IRReg
    = VirtualReg IR.VarID
    | ConcreteReg Register
    deriving (Ord, Eq, Generic)

instance Show X86IRReg where
    show (VirtualReg vid) = show vid
    show (ConcreteReg reg) = "%" ++ show reg

instance Hashable X86IRReg

createX86IRBB :: IR.BasicBlock IR.VarID -> IR.BasicBlock X86IRReg
createX86IRBB (IR.BasicBlock name is) = IR.BasicBlock name x86is
    where
        x86is :: Seq (IR.Instruction X86IRReg)
        x86is = x86 is

        translate :: Functor f => f IR.VarID -> f X86IRReg
        translate = (VirtualReg <$>)

        x86 :: Seq (IR.Instruction IR.VarID) -> Seq (IR.Instruction X86IRReg)
        -- x86 multiply uses rax as one operand always
        x86 ((IR.Mul res l r) :<| rest) =
            (IR.Move rax (translate l)) :<|                     -- mov lhs -> rax
            (IR.Mul rax (IR.Variable rax) (translate r)) :<|    -- rax := rax * rhs
            (IR.Move (VirtualReg res) (IR.Variable rax)) :<|    -- mov rax -> res
            x86 rest
        -- x86 division uses rax as left operand always
        x86 ((IR.Div res l r) :<| rest) =
            (IR.Move rax (translate l)) :<|                     -- mov lhs -> rax
            (IR.Div rax (IR.Variable rax) (translate r)) :<|    -- rax := rax / rhs
            (IR.Move (VirtualReg res) (IR.Variable rax)) :<|    -- mov rax -> res
            x86 rest
        -- x86 calling convention - first argument should be in
        -- rdi, result returned always in rax
        x86 ((IR.Call res fun arg) :<| rest) =
            (IR.Move rdi (translate arg)) :<|                   -- mov arg -> rdi
            (IR.Call rax (translate fun) (IR.Variable rdi)) :<| -- call <fun>
            (IR.Move (VirtualReg res) (IR.Variable rax)) :<|    -- mov rax -> res
            x86 rest
        x86 ((IR.Ret res) :<| rest) =
            (IR.Move rax (translate res)) :<|                   -- mov res -> rax
            (IR.Ret (IR.Variable rax)) :<|                      -- ret
            x86 rest

        -- TODO: Conditionals

        -- Default case: Instructions which don't have 
        -- any special requirements are just mapped to use
        -- virtual registers
        x86 (i :<| rest) = translate i :<| x86 rest
        x86 Seq.Empty = Seq.Empty

        rax, rdi :: X86IRReg
        rax = ConcreteReg RAX
        rdi = ConcreteReg RDI

createX86IRFunc :: IR.Function IR.VarID -> IR.Function X86IRReg
createX86IRFunc (IR.Function name bs) = IR.Function name (createX86IRBB <$> bs)

allocateRegisters :: IR.Program IR.VarID -> IR.Program Register
allocateRegisters (IR.Program fs) = IR.Program $ fAlloc <$> fs
    where
        fAlloc :: IR.Function IR.VarID -> IR.Function Register
        fAlloc func@(IR.Function name _) = IR.Function name $ undefined
            where
                x86func :: IR.Function X86IRReg
                x86func = createX86IRFunc func

                flowGraph :: IR.FlowGraph (IR.BasicBlock X86IRReg)
                flowGraph = IR.createFlowGraph x86func

                liveVars :: IR.FlowGraph (IR.LVABasicBlock X86IRReg)
                (_, liveVars) = IR.findLiveVarsDAG flowGraph

                clashGraph :: IR.ClashGraph X86IRReg
                clashGraph = IR.createClashGraph liveVars

                prefGraph :: IR.PreferenceGraph X86IRReg
                prefGraph = IR.createPrefGraph flowGraph

                -- Create the virtual register stack by repeatedly removing the
                -- vertex with fewest edges in the clash graph
                -- TODO: Handle spilling
                -- Pop virtual registers off the stack, allocating them ideally
                -- to their preference if available, otherwise the next free
                -- concrete register
                -- Repeat until the stack is empty

generateX86 :: IR.Program IR.VarID -> X86_64Code
generateX86 (IR.Program functions) = undefined

