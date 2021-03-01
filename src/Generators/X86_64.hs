{-# LANGUAGE DeriveGeneric #-}

module Generators.X86_64 where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Control.Monad.State
import Data.Maybe (fromJust)
import qualified DataStructs.HashSet as S
import qualified DataStructs.HashMap as M
import Data.Sequence as Seq
import qualified IR

type Label = String

data Register
    = R10       -- General purpose (caller saved)
    | R11       -- General purpose (caller saved)
    | R12       -- General purpose (callee saved)
    | R13       -- General purpose (callee saved)
    | R14       -- General purpose (callee saved)
    | R15       -- General purpose (callee saved)
    | RBX       -- General purpose (callee saved)
    | R9        -- Used for sixth arg
    | R8        -- Used for fifth arg
    | RCX       -- Used for fourth arg
    | RDX       -- Used for third arg
    | RSI       -- Used for second arg
    | RDI       -- Used for first arg
    | RAX       -- Used for return
    deriving (Ord, Eq, Show, Generic)

regAllocOrder :: [Register]
regAllocOrder =
    [ R10
    , R11
    , R12
    , R13
    , R14
    , R15
    , RBX
    , R9
    , R8
    , RCX
    , RDX
    , RSI
    , RDI
    , RAX
    ]

instance Hashable Register

data Immediate
    = I64 Int

data Value
    = Immediate Immediate
    | Register Register

data Instruction
    = Add Value Register
    | Sub Value Register
    | Mul Register
    | Div Register
    | Mov Value Register
    | Call Label
    | Ret
    deriving Show

data Region = Region
    { label :: Label
    , instructions :: Seq Instruction
    }
    deriving Show

data TextSection = TextSection
    { globals :: [Label]
    , externs :: [Label]
    }
    deriving Show

data X86_64Code = X86_64Code
    { textSection :: TextSection
    , regions :: Seq Region
    }
    deriving Show

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
        x86 ((IR.Add res l r) :<| rest) =
            (IR.Move res' (translate l)) :<|                    -- mov lhs -> res
            (IR.Add res' (IR.Variable res') (translate r)) :<|  -- res := res + rhs
            x86 rest
            where res' = VirtualReg res
        x86 ((IR.Sub res l r) :<| rest) =
            (IR.Move res' (translate l)) :<|                    -- mov lhs -> res
            (IR.Sub res' (IR.Variable res') (translate r)) :<|  -- res := res - rhs
            x86 rest
            where res' = VirtualReg res
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
        fAlloc func = applyAlloc <$> x86func
            where
                applyAlloc :: X86IRReg -> Register
                applyAlloc (ConcreteReg r) = r
                applyAlloc (VirtualReg v) = 
                    case M.lookup v vmap of
                        Just r -> r
                        Nothing -> error "ERROR!"
                    
                    where
                        vstack = evalState createStack clashGraph
                        vmap = allocator vstack

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
                createStack :: State (IR.ClashGraph X86IRReg) [IR.VarID]
                createStack = createStack' []
                    where
                        createStack' acc = do
                            minVert <- popMinVertex
                            case minVert of
                                Nothing -> pure acc
                                Just r -> createStack' (r:acc)

                popMinVertex :: State (IR.ClashGraph X86IRReg) (Maybe IR.VarID)
                popMinVertex = do
                    graph <- get
                    let mv = findMin graph
                    modify $ popNode mv
                    pure mv

                    where
                        findMin :: IR.ClashGraph X86IRReg -> Maybe IR.VarID
                        findMin (IR.ClashGraph graph) = fst <$> M.foldlWithKey combine Nothing graph
                            where
                                combine :: Maybe (IR.VarID, Int) -> X86IRReg -> S.HashSet X86IRReg 
                                        -> Maybe (IR.VarID, Int)
                                -- Ignore nodes which already have concrete registers
                                combine best (ConcreteReg _) _ = best
                                -- If we don't yet have a best, set it to this
                                combine Nothing (VirtualReg v) s = Just (v, S.size s)
                                -- Otherwise, check if this node has less edges than the best
                                combine best@(Just (_, sz)) (VirtualReg v') s
                                    | sz' < sz = Just (v', sz')
                                    | otherwise = best
                                    where
                                        sz' = S.size s

                        popNode :: Maybe IR.VarID -> IR.ClashGraph X86IRReg -> IR.ClashGraph X86IRReg
                        popNode Nothing g = g
                        popNode (Just v) (IR.ClashGraph graph) = IR.ClashGraph $ (dropConns . dropNode) graph
                            where
                                dropConns = M.map (S.delete v')
                                dropNode = M.delete v'

                                v' :: X86IRReg
                                v' = VirtualReg v
                
                allocator :: [IR.VarID] -> M.HashMap IR.VarID Register
                allocator = allocator' M.empty
                    where
                        allocator' acc [] = acc
                        allocator' acc (v:vs) = allocator' (M.insert v reg acc) vs
                            where
                                reg :: Register
                                reg =
                                    case M.lookup virtV (IR.preferences prefGraph) of
                                        Just ns -> case foldl findMatch Nothing ns of
                                            Just r -> r
                                            Nothing -> allocNext
                                        Nothing -> allocNext

                                allocNext :: Register
                                allocNext = allocNext' regAllocOrder
                                    where
                                        allocNext' [] = error "SPILLED! TODO: Add spilling"
                                        allocNext' (r:rs) = 
                                            case checkNeighbours neighbours r of
                                                Just _ -> r
                                                Nothing -> allocNext' rs

                                findMatch :: Maybe Register -> X86IRReg -> Maybe Register
                                findMatch Nothing (ConcreteReg r) = 
                                    checkNeighbours neighbours r
                                findMatch Nothing r@(VirtualReg v') = do
                                    ideal <- M.lookup v' acc
                                    checkNeighbours neighbours ideal
                                findMatch best _ = best

                                checkNeighbours :: Maybe (S.HashSet X86IRReg) -> Register -> Maybe Register
                                checkNeighbours Nothing ideal = Just ideal
                                checkNeighbours (Just ns) ideal = foldl checkClash (Just ideal) ns
                                    
                                checkClash :: Maybe Register -> X86IRReg -> Maybe Register
                                checkClash Nothing _ = Nothing
                                checkClash reg@(Just r) (ConcreteReg r')
                                    | r == r' = Nothing
                                    | otherwise = reg
                                checkClash reg@(Just r) (VirtualReg v) = 
                                    case M.lookup v acc of
                                        Nothing -> reg
                                        Just r'
                                            | r == r' -> Nothing
                                            | otherwise -> reg

                                virtV :: X86IRReg
                                virtV = VirtualReg v

                                neighbours :: Maybe (S.HashSet X86IRReg)
                                neighbours = M.lookup virtV $ IR.clashes clashGraph

generateX86 :: IR.Program IR.VarID -> X86_64Code
generateX86 prog = X86_64Code text blocks
    where
        x86prog = allocateRegisters prog

        text :: TextSection
        text = TextSection [] (map IR.fid $ M.elems $ IR.funcs x86prog)

        blocks :: Seq Region
        blocks = foldl (><) Seq.Empty $ map emitFunc $ M.elems $ IR.funcs x86prog

        emitFunc :: IR.Function Register -> Seq Region
        emitFunc (IR.Function name bs) = (Region name Seq.Empty) :<| (mapBlock <$> bs)

        mapBlock :: IR.BasicBlock Register -> Region
        mapBlock (IR.BasicBlock lab is) = Region lab $ emit is

        emit ((IR.Move dst (IR.Variable src)) :<| rest)
            | src == dst = emit rest                -- skip over redundant moves
            | otherwise = Mov src dst :<| emit rest
        emit ((IR.Add res _ (IR.Variable r)) :<| rest) =
            (Add r res) :<|
            emit rest
        emit ((IR.Sub res _ (IR.Variable r)) :<| rest) =
            (Sub r res) :<|
            emit rest
        emit ((IR.Mul _ _ (IR.Variable r)) :<| rest) =
            (Mul r) :<|
            emit rest
        emit ((IR.Div _ _ (IR.Variable r)) :<| rest) =
            (Div r) :<|
            emit rest
        emit ((IR.Ret _) :<| rest) =
            (Ret) :<|
            emit rest

        -- TODO: Implement the rest of the code generators
        emit (i :<| _) = error $ "NOT IMPLEMENTED YET! " ++ show i

        emit Seq.Empty = Seq.Empty
            

