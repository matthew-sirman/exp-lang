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

import Debug.Trace

type Label = String

data RSize
    = Q8
    | E4
    | W2
    | B1
    deriving (Ord, Eq, Generic)

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
    deriving (Ord, Eq, Generic)

data RegAccess = RegAccess RSize Register
    deriving (Ord, Eq, Generic)

getRegister :: RegAccess -> Register
getRegister (RegAccess _ r) = r

-- Maybe there is a nicer way of doing this?

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

callerSaved :: S.HashSet Register
callerSaved = S.fromList [R10, R11]

calleeSaved :: S.HashSet Register
calleeSaved = S.fromList [R12, R13, R14, R15, RBX]

argRegOrder :: [Register]
argRegOrder = [RDI, RSI, RDX, RCX, R8, R9]

instance Hashable RSize
instance Hashable Register
instance Hashable RegAccess

data Immediate
    = I64 Int

data Value
    = Immediate Immediate
    | Register RegAccess

data Instruction
    = Add Value RegAccess
    | Sub Value RegAccess
    | Mul RegAccess
    | CQTO
    | Div RegAccess
    | Cmp Value Value
    | SetEQ RegAccess
    | SetLT RegAccess
    | SetGT RegAccess
    | SetLE RegAccess
    | SetGE RegAccess
    | Mov Value RegAccess
    | LEA Label RegAccess
    | Write Value RegAccess
    | Read RegAccess RegAccess
    | DCall Label
    | ICall RegAccess
    | JmpE Label
    | Jmp Label
    | Ret
    | Push Value
    | Pop RegAccess

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

data VirtualReg
    = Virtual IR.VarID
    | Fresh IR.VarID
    deriving (Ord, Eq, Generic)

data X86IRReg
    = VirtualReg VirtualReg
    | ConcreteReg Register
    deriving (Ord, Eq, Generic)

data X86IRSizedReg = X86IRSizedReg RSize X86IRReg
    deriving (Ord, Eq, Generic)

getIRRegister :: X86IRSizedReg -> X86IRReg
getIRRegister (X86IRSizedReg _ r) = r

instance Hashable VirtualReg
instance Hashable X86IRReg
instance Hashable X86IRSizedReg

data BasicBlockState = BasicBlockState (M.HashMap IR.Label (X86IRSizedReg, IR.Value X86IRSizedReg)) IR.VarID

createX86IRBB :: M.HashMap IR.VarID X86IRSizedReg -> IR.BasicBlock IR.VarID
              -> State BasicBlockState (IR.BasicBlock X86IRSizedReg)
createX86IRBB fixedMap (IR.BasicBlock name is) = do
    x86is <- x86 is
    pure $ IR.BasicBlock name x86is
    where
        translate :: IR.Value IR.VarID -> IR.Value X86IRSizedReg
        translate (IR.Immediate imm) = IR.Immediate imm
        translate (IR.Variable v) = case M.lookup v fixedMap of
            Just r -> IR.Variable r
            Nothing -> IR.Variable (X86IRSizedReg Q8 $ VirtualReg (Virtual v))
        translate (IR.Closure cl) = IR.Closure ((\r -> X86IRSizedReg Q8 $ VirtualReg (Virtual r)) <$> cl)

        x86 :: Seq (IR.Instruction IR.VarID) -> State BasicBlockState (Seq (IR.Instruction X86IRSizedReg))
        x86 ((IR.Add res l r) :<| rest) = do
            let res' = X86IRSizedReg Q8 $ VirtualReg (Virtual res)
                mov = IR.Move res' (translate l)
                add = IR.Add res' (IR.Variable res') (translate r)
            rest' <- x86 rest
            pure $ mov :<| add :<| rest'

        x86 ((IR.Sub res l r) :<| rest) = do
            let res' = X86IRSizedReg Q8 $ VirtualReg (Virtual res)
                mov = IR.Move res' (translate l)
                sub = IR.Sub res' (IR.Variable res') (translate r)
            rest' <- x86 rest
            pure $ mov :<| sub :<| rest'

        x86 ((IR.Mul res l r) :<| rest) = do
            let res' = X86IRSizedReg Q8 $ VirtualReg (Virtual res)
                movl = IR.Move rax (translate l)
                movr = IR.Move res' (translate r)
                mul = IR.Mul rax (IR.Variable rax) (IR.Variable res')
            rest' <- x86 rest
            pure $ movl :<| movr :<| mul :<| rest'

        x86 ((IR.Div res l r) :<| rest) = do
            let res' = X86IRSizedReg Q8 $ VirtualReg (Virtual res)
                movl = IR.Move rax (translate l)
                movr = IR.Move res' (translate r)
                div = IR.Div rax (IR.Variable rax) (IR.Variable res')
            rest' <- x86 rest
            pure $ movl :<| movr :<| div :<| rest'

        x86 ((IR.EQ res l r) :<| rest) = x86cmpOp IR.EQ res l r rest
        x86 ((IR.LT res l r) :<| rest) = x86cmpOp IR.LT res l r rest
        x86 ((IR.GT res l r) :<| rest) = x86cmpOp IR.GT res l r rest
        x86 ((IR.LE res l r) :<| rest) = x86cmpOp IR.LE res l r rest
        x86 ((IR.GE res l r) :<| rest) = x86cmpOp IR.GE res l r rest

        x86 ((IR.Move res v) :<| rest) = do
            let res' = X86IRSizedReg Q8 $ VirtualReg (Virtual res)
                mov = IR.Move res' (translate v)
            rest' <- x86 rest
            pure $ mov :<| rest'

        x86 ((IR.Write c@(IR.Closure _) to size) :<| rest) = do
            fresh <- freshReg Q8
            let mov = IR.Move fresh (translate c)
                wr = IR.Write (IR.Variable fresh) (translate to) size
            rest' <- x86 rest
            pure $ mov :<| wr :<| rest'

        x86 ((IR.Write from to size) :<| rest) = do
            let wr = IR.Write (translate from) (translate to) size
            rest' <- x86 rest
            pure $ wr :<| rest'

        x86 ((IR.Read res a size) :<| rest) = do
            let res' = X86IRSizedReg Q8 $ VirtualReg (Virtual res)
                rd = IR.Read res' (translate a) size
            rest' <- x86 rest
            pure $ rd :<| rest'

        x86 ((IR.MAlloc res v) :<| rest) = do
            let res' = X86IRSizedReg Q8 $ VirtualReg (Virtual res)
                movI = IR.Move rdi (translate v)
                call = IR.Call rax (IR.Closure (IR.FClosure "malloc" Nothing)) [IR.Variable rdi]
                movO = IR.Move res' (IR.Variable rax)
            rest' <- x86 rest
            pure $ movI :<| call :<| movO :<| rest'

        -- x86 calling convention - first argument should be in
        -- rdi, result returned always in rax
        -- TODO: Add PROPER support for multiple args!
        x86 ((IR.Call res fun [arg]) :<| rest) = do
            let res' = X86IRSizedReg Q8 $ VirtualReg (Virtual res)
                movI = IR.Move rdi (translate arg)
                call = IR.Call rax (translate fun) [IR.Variable rdi]
                movO = IR.Move res' (IR.Variable rax)
            rest' <- x86 rest
            pure $ movI :<| call :<| movO :<| rest'

        x86 ((IR.Call res fun [arg1, arg2]) :<| rest) = do
            let res' = X86IRSizedReg Q8 $ VirtualReg (Virtual res)
                movI0 = IR.Move rdi (translate arg1)
                movI1 = IR.Move rsi (translate arg2)
                call = IR.Call rax (translate fun) [IR.Variable rdi, IR.Variable rsi]
                movO = IR.Move res' (IR.Variable rax)
            rest' <- x86 rest
            pure $ movI0 :<| movI1 :<| call :<| movO :<| rest'

        x86 ((IR.Branch val lab) :<| rest) = do
            let br = IR.Branch (translate val) lab
            rest' <- x86 rest
            pure $ br :<| rest'

        x86 ((IR.Jump lab) :<| rest) = do
            let jmp = IR.Jump lab
            rest' <- x86 rest
            pure $ jmp :<| rest'

        x86 ((IR.Phi res (vl, ll) (vr, lr)) :<| rest) = do
            modify addPhiEntries
            x86 rest

            where
                addPhiEntries :: BasicBlockState -> BasicBlockState
                addPhiEntries (BasicBlockState phiMap vs) = BasicBlockState
                    (M.insert ll (res', translate vl) $ M.insert lr (res', translate vr) phiMap) vs
                res' = X86IRSizedReg Q8 $ VirtualReg (Virtual res)

        x86 ((IR.Ret res) :<| rest) = do
            let mov = IR.Move rax (translate res)
                ret = IR.Ret (IR.Variable rax)
            rest' <- x86 rest
            pure $ mov :<| ret :<| rest'

        x86 ((IR.Push v) :<| rest) = do
            let push = IR.Push (translate v)
            rest' <- x86 rest
            pure $ push :<| rest'

        x86 ((IR.Pop res) :<| rest) = do
            let res' = X86IRSizedReg Q8 $ VirtualReg (Virtual res)
                pop = IR.Pop res'
            rest' <- x86 rest
            pure $ pop :<| rest'

        x86 Seq.Empty = pure Seq.Empty

        x86cmpOp :: (X86IRSizedReg -> IR.Value X86IRSizedReg -> IR.Value X86IRSizedReg 
                    -> IR.Instruction X86IRSizedReg)
                 -> IR.VarID -> IR.Value IR.VarID -> IR.Value IR.VarID 
                 -> Seq (IR.Instruction IR.VarID) -> State BasicBlockState (Seq (IR.Instruction X86IRSizedReg))
        x86cmpOp op res l r rest = do
            fresh <- freshReg Q8
            let res' = X86IRSizedReg B1 $ VirtualReg (Virtual res)
                mov = IR.Move fresh (translate r)
                op' = op res' (translate l) (IR.Variable fresh)
            rest' <- x86 rest
            pure $ mov :<| op' :<| rest'

        freshReg :: RSize -> State BasicBlockState X86IRSizedReg
        freshReg sz = do
            (BasicBlockState phiMap vs) <- get
            put (BasicBlockState phiMap (vs + 1))
            pure $ X86IRSizedReg sz $ VirtualReg (Fresh (vs + 1))

        rax, rdi, rsi :: X86IRSizedReg
        rax = X86IRSizedReg Q8 $ ConcreteReg RAX
        rdi = X86IRSizedReg Q8 $ ConcreteReg RDI
        rsi = X86IRSizedReg Q8 $ ConcreteReg RSI

createX86IRFunc :: IR.Function IR.VarID -> IR.Function X86IRSizedReg
createX86IRFunc (IR.Function name as bs) = IR.Function name x86args (addPhiCoalescences x86bs)
    where
        x86args :: [X86IRSizedReg]
        x86args = Prelude.take (Prelude.length as) $ map (X86IRSizedReg Q8 . ConcreteReg) argRegOrder

        argMap :: M.HashMap IR.VarID X86IRSizedReg
        argMap = M.fromList $ Prelude.zip as x86args

        x86bs :: Seq (IR.BasicBlock X86IRSizedReg)
        phiMap :: M.HashMap IR.Label (X86IRSizedReg, IR.Value X86IRSizedReg)
        (x86bs, (BasicBlockState phiMap _)) = runState (mapM (createX86IRBB argMap) bs) (BasicBlockState M.empty 0)

        addPhiCoalescences :: Seq (IR.BasicBlock X86IRSizedReg) -> Seq (IR.BasicBlock X86IRSizedReg)
        addPhiCoalescences Seq.Empty = Seq.Empty
        addPhiCoalescences (bb@(IR.BasicBlock name is) :<| rest) =
            case M.lookup name phiMap of
                Nothing -> bb :<| addPhiCoalescences rest
                Just (res, val) -> (IR.BasicBlock name (cIns (IR.Move res val) is)) :<| addPhiCoalescences rest
            where
                cIns c (heads :|> last)
                    | branches last = heads :|> c :|> last
                    | otherwise = heads :|> last :|> c
                cIns c _ = is :|> c
                branches (IR.Branch _ _) = True
                branches (IR.Jump _) = True
                branches _ = False

allocateWithSaves :: (X86IRReg -> Register)
                  -> IR.NodeIDMap 
                  -> IR.FlowGraph (IR.LVABasicBlock X86IRReg)
                  -> S.HashSet X86IRReg
                  -> IR.Function X86IRSizedReg
                  -> IR.Function RegAccess
allocateWithSaves allocator nodeIDMap liveVars allVars (IR.Function name as bs) = IR.Function name newAs newBs
    where
        newAs :: [RegAccess]
        newAs = sizeAlloc <$> as
        
        newBs :: Seq (IR.BasicBlock RegAccess)
        newBs = entryBlock :<| (addCallerSaves <$> bs)

        -- Get the set of all registers used in this function which need to be saved
        -- and convert to an ordered list (the ordering is irrelevant, however)
        saveVars :: [Register]
        saveVars = S.toList $ S.map allocator allVars `S.intersection` calleeSaved

        sizeAlloc :: X86IRSizedReg -> RegAccess
        sizeAlloc (X86IRSizedReg sz r) = RegAccess sz $ allocator r

        entryBlock :: IR.BasicBlock RegAccess
        entryBlock = IR.BasicBlock (name ++ "_entry") (is saveVars)
            where
                is :: [Register] -> Seq (IR.Instruction RegAccess)
                is [] = Seq.Empty
                is (r:rs) = (IR.Push (IR.Variable (RegAccess Q8 r))) :<| is rs

        addCallerSaves :: IR.BasicBlock X86IRSizedReg -> IR.BasicBlock RegAccess
        addCallerSaves (IR.BasicBlock lab is) = IR.BasicBlock lab (alloc $ Seq.zip is blockLVs)
            where
                blockLVs :: Seq (S.HashSet X86IRReg)
                blockLVs = IR.liveVars $ blockUnwrap $ M.lookup nID (IR.nodes liveVars)
                    where
                        nID = fromJust $ M.lookup lab nodeIDMap
                        blockUnwrap :: Maybe (IR.Node r) -> r
                        blockUnwrap (Just (IR.Node (IR.BlockNode b) _ _)) = b
                
                alloc :: Seq (IR.Instruction X86IRSizedReg, S.HashSet X86IRReg) -> Seq (IR.Instruction RegAccess)
                alloc Seq.Empty = Seq.Empty
                alloc ((i@(IR.Call _ _ _), lvs) :<| rest) = 
                    addPushes clashLVs ((sizeAlloc <$> i) :<| addPops clashLVs (alloc rest))
                    where
                        clashLVs = S.toList $ (S.map allocator lvs) `S.intersection` callerSaved
                alloc ((i@(IR.Ret _), _) :<| rest) = addPops saveVars ((sizeAlloc <$> i) :<| alloc rest)
                alloc ((i, _) :<| rest) = (sizeAlloc <$> i) :<| alloc rest

                -- Add pops and pushes in reverse orders
                addPushes :: [Register] -> Seq (IR.Instruction RegAccess) -> Seq (IR.Instruction RegAccess)
                addPushes [] acc = acc
                addPushes (r:rs) acc = (IR.Push (IR.Variable (RegAccess Q8 r))) :<| addPushes rs acc

                addPops :: [Register] -> Seq (IR.Instruction RegAccess) -> Seq (IR.Instruction RegAccess)
                addPops [] acc = acc
                addPops (r:rs) acc = addPops rs ((IR.Pop (RegAccess Q8 r)) :<| acc)

                iAlloc :: IR.Instruction X86IRSizedReg -> IR.Instruction RegAccess
                iAlloc = fmap sizeAlloc

allocateRegisters :: IR.Program IR.VarID -> IR.Program RegAccess
allocateRegisters (IR.Program fs) = IR.Program $ fAlloc <$> fs
    where
        fAlloc :: IR.Function IR.VarID -> IR.Function RegAccess
        fAlloc func = allocateWithSaves applyAlloc nodeIDMap liveVars allVars x86func
            where
                applyAlloc :: X86IRReg -> Register
                applyAlloc (ConcreteReg r) = r
                applyAlloc (VirtualReg v) = 
                    case M.lookup v vmap of
                        Just r -> r
                        Nothing -> error $ "Register allocation failed!"
                    
                vstack :: [VirtualReg]
                vstack = evalState createStack clashGraph

                vmap :: M.HashMap VirtualReg Register
                vmap = allocator vstack

                -- Clash and preference graph creation
                x86func :: IR.Function X86IRSizedReg
                x86func = createX86IRFunc func

                flowGraph :: IR.FlowGraph (IR.BasicBlock X86IRReg)
                nodeIDMap :: IR.NodeIDMap
                (flowGraph, nodeIDMap) = IR.createFlowGraph (getIRRegister <$> x86func)

                liveVars :: IR.FlowGraph (IR.LVABasicBlock X86IRReg)
                (_, liveVars) = IR.findLiveVarsDAG flowGraph

                allVars :: S.HashSet X86IRReg
                allVars = S.map getIRRegister $ IR.findAllVars x86func

                clashGraph :: IR.ClashGraph X86IRReg
                clashGraph = IR.ClashGraph $ M.unionWith const graph emptyClashes
                    where
                        -- This is kind of ugly...
                        -- Create the clash graph
                        (IR.ClashGraph graph) = IR.createClashGraph liveVars
                        -- Create an empty clash graph to fill in missing variables
                        emptyClashes = M.fromList $ map (\v -> (v, S.empty)) $ S.toList allVars

                prefGraph :: IR.PreferenceGraph X86IRReg
                prefGraph = IR.createPrefGraph flowGraph

                -- Create the virtual register stack by repeatedly removing the
                -- vertex with fewest edges in the clash graph
                -- TODO: Handle spilling
                -- Pop virtual registers off the stack, allocating them ideally
                -- to their preference if available, otherwise the next free
                -- concrete register
                -- Repeat until the stack is empty
                createStack :: State (IR.ClashGraph X86IRReg) [VirtualReg]
                createStack = createStack' []
                    where
                        createStack' acc = do
                            minVert <- popMinVertex
                            case minVert of
                                Nothing -> pure acc
                                Just r -> createStack' (r:acc)

                popMinVertex :: State (IR.ClashGraph X86IRReg) (Maybe VirtualReg)
                popMinVertex = do
                    graph <- get
                    let mv = findMin graph
                    modify $ popNode mv
                    pure mv

                    where
                        findMin :: IR.ClashGraph X86IRReg -> Maybe VirtualReg
                        findMin (IR.ClashGraph graph) = fst <$> M.foldlWithKey combine Nothing graph
                            where
                                combine :: Maybe (VirtualReg, Int) -> X86IRReg -> S.HashSet X86IRReg 
                                        -> Maybe (VirtualReg, Int)
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

                        popNode :: Maybe VirtualReg -> IR.ClashGraph X86IRReg -> IR.ClashGraph X86IRReg
                        popNode Nothing g = g
                        popNode (Just v) (IR.ClashGraph graph) = IR.ClashGraph $ (dropConns . dropNode) graph
                            where
                                dropConns = M.map (S.delete v')
                                dropNode = M.delete v'

                                v' :: X86IRReg
                                v' = VirtualReg v
                
                allocator :: [VirtualReg] -> M.HashMap VirtualReg Register
                allocator = allocator' M.empty
                    where
                        allocator' :: M.HashMap VirtualReg Register -> [VirtualReg] -> M.HashMap VirtualReg Register
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
        x86prog :: IR.Program RegAccess
        x86prog = allocateRegisters prog

        text :: TextSection
        text = TextSection (map IR.fid $ M.elems $ IR.funcs x86prog) ["malloc"]

        blocks :: Seq Region
        blocks = foldl (><) Seq.Empty $ map emitFunc $ M.elems $ IR.funcs x86prog

        emitFunc :: IR.Function RegAccess -> Seq Region
        emitFunc (IR.Function name _ bs) = (Region name Seq.Empty) :<| (mapBlock <$> bs)

        mapBlock :: IR.BasicBlock RegAccess -> Region
        mapBlock (IR.BasicBlock lab is) = Region lab $ emit is

        valEmit :: IR.Value RegAccess -> Value
        valEmit (IR.Immediate (IR.Int64 i)) = Immediate $ I64 i
        valEmit (IR.Immediate (IR.Bool True)) = Immediate $ I64 1
        valEmit (IR.Immediate (IR.Bool False)) = Immediate $ I64 0
        valEmit (IR.Variable reg) = Register reg
        valEmit (IR.Closure (IR.FClosure _ (Just r))) = Register r
        -- valEmit (IR.Closure (IR.FClosure lab Nothing)) = Label lab
        -- TODO: Fix closures
        -- valEmit (IR.Closure

        emit :: Seq (IR.Instruction RegAccess) -> Seq Instruction
        emit ((IR.Add res _ r) :<| rest) =
            (Add (valEmit r) res) :<|
            emit rest
        emit ((IR.Sub res _ r) :<| rest) =
            (Sub (valEmit r) res) :<|
            emit rest
        emit ((IR.Mul _ _ (IR.Variable r)) :<| rest) =
            (Mul r) :<|
            emit rest
        emit ((IR.Div _ _ (IR.Variable r)) :<| rest) =
            (CQTO) :<|
            (Div r) :<|
            emit rest
        emit ((IR.EQ res l r) :<| rest) =
            (Cmp (valEmit l) (valEmit r)) :<|
            (SetEQ res) :<|
            emit rest
        emit ((IR.LT res l r) :<| rest) =
            (Cmp (valEmit l) (valEmit r)) :<|
            (SetLT res) :<|
            emit rest
        emit ((IR.GT res l r) :<| rest) =
            (Cmp (valEmit l) (valEmit r)) :<|
            (SetGT res) :<|
            emit rest
        emit ((IR.LE res l r) :<| rest) =
            (Cmp (valEmit l) (valEmit r)) :<|
            (SetLE res) :<|
            emit rest
        emit ((IR.GE res l r) :<| rest) =
            (Cmp (valEmit l) (valEmit r)) :<|
            (SetGE res) :<|
            emit rest
        emit ((IR.Move dst (IR.Variable src)) :<| rest)
            | src == dst = emit rest                -- skip over redundant moves
        emit ((IR.Move dst (IR.Closure (IR.FClosure lab Nothing))) :<| rest) =
            LEA lab dst :<|
            emit rest
        emit ((IR.Move dst src) :<| rest) = 
            Mov (valEmit src) dst :<| 
            emit rest
        emit ((IR.Write v (IR.Variable r) _) :<| rest) =
            (Write (valEmit v) r) :<|
            emit rest
        emit ((IR.Read res (IR.Variable v) _) :<| rest) =
            (Read v res) :<|
            emit rest
        emit ((IR.Call _ (IR.Closure (IR.FClosure name _)) _) :<| rest) =
            (DCall name) :<|
            emit rest
        emit ((IR.Call _ (IR.Variable v) _) :<| rest) =
            (ICall v) :<|
            emit rest
        emit ((IR.Branch val lab) :<| rest) = 
            (Cmp (Immediate $ I64 0) (valEmit val)) :<|
            (JmpE lab) :<|
            emit rest
        emit ((IR.Jump lab) :<| rest) =
            (Jmp lab) :<|
            emit rest
        emit ((IR.Ret _) :<| rest) =
            (Ret) :<|
            emit rest
        emit ((IR.Push v) :<| rest) = 
            (Push (valEmit v)) :<|
            emit rest
        emit ((IR.Pop r) :<| rest) =
            (Pop r) :<|
            emit rest

        -- TODO: Implement the rest of the code generators
        emit (i :<| _) = error $ "NOT IMPLEMENTED YET! "

        emit Seq.Empty = Seq.Empty
            

