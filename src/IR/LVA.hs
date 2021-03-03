{-# LANGUAGE ScopedTypeVariables #-}

module IR.LVA (
    LVABasicBlock(..)
  , ClashGraph(..)
  , PreferenceGraph(..)
  , findLiveVarsDAG
  , createClashGraph
  , createPrefGraph
) where

import qualified DataStructs.HashSet as S
import qualified DataStructs.HashMap as M
import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Foldable (toList)
import Data.Sequence as Seq hiding (intersperse)
import Data.List (intersperse)
import IR.Instruction as IR
import IR.BasicBlock as IR
import IR.Function as IR
import IR.FlowGraph as IR

data LVABasicBlock r = LVABasicBlock
    { lvaBBLabel :: IR.Label
    , liveVars :: Seq (S.HashSet r)
    }

instance Labelled (LVABasicBlock r) where
    getLabel = lvaBBLabel
    
instance Show r => Show (LVABasicBlock r) where
    show (LVABasicBlock lab vars) =
        lab ++ ":\n"
            ++ concat (toList $ fmap (\s -> "    " ++ showSet s ++ "\n") vars)
        where
            showSet s = "{" ++ concat (intersperse ", " $ map show $ toList s) ++ "}"

type Graph a = M.HashMap a (S.HashSet a)

-- Data structure for an undirected register clash graph
-- There is an edge present between nodes A and B if they
-- appear simultaneously in any live variable set
newtype ClashGraph r = ClashGraph
    { clashes :: Graph r
    }
    deriving Show

-- Data structure for an undirected preference graph
-- There is an edge present between nodes A and B if we
-- would ideally like to allocate them the same register
newtype PreferenceGraph r = PreferenceGraph
    { preferences :: Graph r
    }
    deriving Show

-- Should be semantically equivalent to foldl . reverse, but
-- more efficient with sequences
reverseFoldl :: (b -> a -> b) -> b -> Seq a -> b
reverseFoldl f e Seq.Empty = e
reverseFoldl f e (xs :|> x) = reverseFoldl f (f e x) xs

findBBLiveVars :: forall r. (Ord r, Eq r, M.Hashable r) 
               => S.HashSet r -> IR.BasicBlock r -> LVABasicBlock r
findBBLiveVars varsAfter (IR.BasicBlock l is) = LVABasicBlock l lvs
    where
        lvs :: Seq (S.HashSet r)
        -- Fold in reverse (so we have access to two elements at once) and 
        -- build up a sequence of sets of live variables. This
        -- works from the end backwards, hence the reverse fold.
        lvs = reverseFoldl combine (Seq.singleton varsAfter) is

        -- Combiner function - look at the next set of live vars after this
        -- node. The live vars before this node are is the difference between 
        -- the live vars after the node and any variables defined here, unioned
        -- with any variable referenced here.
        combine :: Seq (S.HashSet r) -> IR.Instruction r -> Seq (S.HashSet r)
        combine (l :<| ls) i = ((l S.\\ def i) `S.union` ref i) :<| (l :<| ls)

        -- Ref - the set of all variables referenced by this instruction
        ref :: IR.Instruction r -> S.HashSet r
        ref (IR.Add _ vl vr) = valRef vl `S.union` valRef vr
        ref (IR.Sub _ vl vr) = valRef vl `S.union` valRef vr
        ref (IR.Mul _ vl vr) = valRef vl `S.union` valRef vr
        ref (IR.Div _ vl vr) = valRef vl `S.union` valRef vr
        ref (IR.EQ _ vl vr) = valRef vl `S.union` valRef vr
        ref (IR.LT _ vl vr) = valRef vl `S.union` valRef vr
        ref (IR.GT _ vl vr) = valRef vl `S.union` valRef vr
        ref (IR.LE _ vl vr) = valRef vl `S.union` valRef vr
        ref (IR.GE _ vl vr) = valRef vl `S.union` valRef vr
        ref (IR.Move _ v) = valRef v
        ref (IR.Call _ _ vas) = (S.unions $ map valRef vas)
        ref (IR.Branch v _) = valRef v
        ref (IR.Jump _) = S.empty
        ref (IR.Phi _ (vl, _) (vr, _)) = valRef vl `S.union` valRef vr
        ref (IR.Ret v) = valRef v

        -- Helper for getting the reference set from a value
        valRef :: IR.Value r -> S.HashSet r
        valRef (IR.Variable v) = S.singleton v
        valRef _ = S.empty

        -- Def - the set of all variables defined by this instruction
        def :: IR.Instruction r -> S.HashSet r
        def (IR.Add v _ _) = S.singleton v
        def (IR.Sub v _ _) = S.singleton v
        def (IR.Mul v _ _) = S.singleton v
        def (IR.Div v _ _) = S.singleton v
        def (IR.EQ v _ _) = S.singleton v
        def (IR.LT v _ _) = S.singleton v
        def (IR.GT v _ _) = S.singleton v
        def (IR.LE v _ _) = S.singleton v
        def (IR.GE v _ _) = S.singleton v
        def (IR.Move v _) = S.singleton v
        def (IR.Call v _ _) = S.singleton v
        def (IR.Branch _ _) = S.empty
        def (IR.Jump _) = S.empty
        def (IR.Phi v _ _) = S.singleton v
        def (IR.Ret _) = S.empty

-- The state keeps a memoised map from nodes to their before-LV set, and
-- the current map from nodes to LVA basic blocks
type LVState r = (M.HashMap IR.NodeID (S.HashSet r), M.HashMap IR.NodeID (IR.Node (LVABasicBlock r)))

-- Find the live variables assuming the input is a DAG - there 
-- are no loops. This way, we can find the live variables in a single
-- pass
-- We wish to convert a flow graph of basic blocks, with register model "r" into a 
-- graph of LVA basic blocks, with in the same register model
findLiveVarsDAG :: forall r. (Ord r, Eq r, M.Hashable r) 
                => IR.FlowGraph (IR.BasicBlock r) -> (S.HashSet r, IR.FlowGraph (LVABasicBlock r))
findLiveVarsDAG (IR.FlowGraph nodes entry exit) = (fvs, IR.FlowGraph lvGraph entry exit)
    where
        lvGraph :: M.HashMap IR.NodeID (IR.Node (LVABasicBlock r)) 
        fvs :: S.HashSet r
        (fvs, (_, lvGraph)) = runState (calcLVs entry (fromJust $ M.lookup entry nodes)) emptyState

        emptyState :: LVState r
        emptyState = (M.empty, M.empty)

        calcLVs :: IR.NodeID -> IR.Node (IR.BasicBlock r) -> State (LVState r) (S.HashSet r)
        -- The exit node starts with no live variables
        calcLVs _ (IR.Node IR.Exit is os) = do
            modify addExit
            pure $ S.empty
            where
                -- No need to memoise the exit node - it is trivially always the empty set
                addExit (memMap, graph) = (memMap, M.insert exit (IR.Node IR.Exit is os) graph)

        calcLVs _ (IR.Node IR.Entry is os) = do
            succs <- S.unions <$> mapM (\n -> calcLVs n (fromJust $ M.lookup n nodes)) (toList os)
            modify addEntry
            -- The entry node carries out no computations, so we can just propagate this value
            -- up. This should in general be an empty set for a closed function.
            pure succs
            where
                -- No need to memoise the entry node either - in fact here we are done so we
                -- don't even care about the memMap
                addEntry (memMap, graph) = (memMap, M.insert entry (IR.Node IR.Entry is os) graph)

        calcLVs nid (IR.Node (IR.BlockNode bb) is os) = do
            memo <- gets $ M.lookup nid . fst
            case memo of
                Just s -> pure s
                Nothing -> calcLiveSet
            where 
                calcLiveSet :: State (LVState r) (S.HashSet r)
                calcLiveSet = do
                    -- Calculate each successors live sets and take the union
                    succs <- S.unions <$> mapM (\n -> calcLVs n (fromJust $ M.lookup n nodes)) (toList os)
                    -- Given the set of live vars after this basic block, calculate the 
                    -- LVABasicBlock, and update the state
                    -- Return the predecessor live variable set for this node
                    updateAndReturn (findBBLiveVars succs bb)
                updateAndReturn :: LVABasicBlock r -> State (LVState r) (S.HashSet r)
                updateAndReturn newBB@(LVABasicBlock _ (predSet :<| _)) = do
                    modify update
                    pure predSet
                    where
                        update :: LVState r -> LVState r
                        update (memMap, graph) = (M.insert nid predSet memMap, M.insert nid newNode graph)
                        newNode = IR.Node (IR.BlockNode newBB) is os

createClashGraph :: forall r. (Eq r, M.Hashable r) => IR.FlowGraph (LVABasicBlock r) -> ClashGraph r
createClashGraph graph = ClashGraph $ M.unionsWith S.union $ map findBBClashes $ M.elems (IR.nodes graph)
    where

        findBBClashes :: IR.Node (LVABasicBlock r) -> Graph r
        -- Entry and exit nodes have no instructions, thus no clashes
        findBBClashes (IR.Node IR.Entry _ _) = M.empty
        findBBClashes (IR.Node IR.Exit _ _) = M.empty
        -- Block nodes: for each instruction, add the clash set, and fold these into one map
        findBBClashes (IR.Node (IR.BlockNode (LVABasicBlock _ lvSets)) _ _) = foldl addClashes M.empty lvSets

        -- To add the clashes for a single instruction, for each element e in the set S
        -- create a mapping from e to (S \ {e}), taking the union if e is already in the
        -- mapping
        addClashes :: Graph r -> S.HashSet r -> Graph r
        addClashes m s = foldl addElem m $ toList s
            where
                addElem :: Graph r -> r -> Graph r
                addElem m' e = M.insertWith S.union e (S.delete e s) m'

createPrefGraph :: forall r. (Eq r, M.Hashable r) => IR.FlowGraph (IR.BasicBlock r) -> PreferenceGraph r
createPrefGraph graph = PreferenceGraph $ M.unionsWith S.union $ map findBBPrefs $ M.elems (IR.nodes graph)
    where
        findBBPrefs :: IR.Node (IR.BasicBlock r) -> Graph r
        -- Entry and exit nodes have no instructions, thus no preferences
        findBBPrefs (IR.Node IR.Entry _ _) = M.empty
        findBBPrefs (IR.Node IR.Exit _ _) = M.empty
        findBBPrefs (IR.Node (IR.BlockNode (IR.BasicBlock _ is)) _ _) = foldl addPrefs M.empty is

        addPrefs :: Graph r -> IR.Instruction r -> Graph r
        -- Add a preference between the source and destination registers in a move
        -- instruction
        addPrefs g (IR.Move dst (IR.Variable src)) = addEdge src dst g
        -- Otherwise, don't add a preference
        addPrefs g _ = g
            
        addEdge :: r -> r -> Graph r -> Graph r
        addEdge a b = (add a b) . (add b a)
            where
                add a' b' = M.insertWith S.union a' (S.singleton b')

