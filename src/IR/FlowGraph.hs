{-# LANGUAGE ScopedTypeVariables #-}

module IR.FlowGraph (
    NodeID
  , NodeType(..)
  , Node(..)
  , FlowGraph(..)
  , createFlowGraph
) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Sequence as Seq hiding (zip)
import Data.Foldable (toList)
import qualified IR.Instruction as IR
import qualified IR.BasicBlock as IR
import qualified IR.Function as IR

type NodeID = Int

data NodeType b
    = BlockNode b -- (IR.BasicBlock r)
    | Entry
    | Exit

instance Show b => Show (NodeType b) where
    show (BlockNode b) = show b
    show Entry = "ENTRY"
    show Exit = "EXIT"

data Node b = Node
    { node :: NodeType b
    , inEdges :: S.Set NodeID
    , outEdges :: S.Set NodeID
    }

instance Show b => Show (Node b) where
    show (Node n inEs outEs) =
        show n ++ "\n"
            ++ "In edges: " ++ show (toList inEs) ++ "\n"
            ++ "Out edges: " ++ show (toList outEs) ++ "\n\n"

data FlowGraph b = FlowGraph
    { nodes :: M.Map NodeID (Node b)
    , entryID :: NodeID
    , exitID :: NodeID
    }

instance Show b => Show (FlowGraph b) where
    show (FlowGraph nodes en ex) =
        concat (map show $ M.elems nodes) ++ "\n\n"
            ++ "Entry: " ++ show en ++ "\n"
            ++ "Exit: " ++ show ex ++ "\n"

addIncomingEdge :: NodeID -> Node b -> Node b
addIncomingEdge e n@(Node _ inEs _) = n { inEdges = S.insert e inEs }

createFlowGraph :: forall r. IR.Function r -> FlowGraph (IR.BasicBlock r)
createFlowGraph (IR.Function _ bs) = FlowGraph graph entryNodeID exitNodeID
    where
        -- Helper map from labels to node IDs
        -- This starts at 1 - the 0th node is the entry node
        bNameMap :: M.Map IR.Label NodeID
        bNameMap = M.fromList $ map (\(IR.BasicBlock lab _, i) -> (lab, i)) $ zip (toList bs) [1..]

        -- Entry node is always 0
        entryNodeID :: NodeID
        entryNodeID = 0

        -- Exit node is always the length of the blocks plus one (e.g. for 3 blocks,
        -- has position 4)
        exitNodeID :: NodeID
        exitNodeID = Seq.length bs + 1

        entryNode :: Node (IR.BasicBlock r)
        entryNode = Node Entry S.empty S.empty

        exitNode :: Node (IR.BasicBlock r)
        exitNode = Node Exit S.empty S.empty
                
        bNameLookup :: IR.Label -> NodeID
        bNameLookup lab = case M.lookup lab bNameMap of
            Just nid -> nid
            Nothing -> error $ "DEV ERROR: Flow graph creation failed to allocate label '" ++ lab ++ "'."

        forwardEdgePass :: M.Map NodeID (Node (IR.BasicBlock r))
        forwardEdgePass = M.fromList $ (exitNodeID, exitNode) : foldl (flip combine) [(entryNodeID, entryNode)] bs
            where
                -- Create the new block, and update the last block in the sequence
                combine :: IR.BasicBlock r -> [(NodeID, Node (IR.BasicBlock r))] -> [(NodeID, Node (IR.BasicBlock r))]
                combine bb@(IR.BasicBlock lab is) ((nid, n) : ns) = (lid, newNode bb is) : (nid, linkPrev lid n) : ns
                    where
                        lid = bNameLookup lab

                -- Link the previous node in the chain to the next node
                linkPrev :: NodeID -> Node (IR.BasicBlock r) -> Node (IR.BasicBlock r)
                -- If the previous node was a basic block, but the final instruction
                -- was an unconditional jump, don't change the node
                linkPrev _ node@(Node (BlockNode (IR.BasicBlock _ (_ :|> IR.Jump _))) _ _) = node
                -- Otherwise, add the next node to the outgoing edges
                linkPrev n (Node nt is os) = Node nt is (S.insert n os)

                -- Create a new node with any nontrivial outgoing edges
                newNode :: IR.BasicBlock r -> Seq (IR.Instruction r) -> Node (IR.BasicBlock r)
                -- If the block ends with a branch, add an outgoing edge
                newNode bb (_ :|> IR.Jump lab) = Node (BlockNode bb) S.empty (S.singleton (bNameLookup lab))
                newNode bb (_ :|> IR.Branch _ lab) = Node (BlockNode bb) S.empty (S.singleton (bNameLookup lab))
                -- If the block ends with a ret, add an outgoing edge to the exit node
                newNode bb (_ :|> IR.Ret _) = Node (BlockNode bb) S.empty (S.singleton exitNodeID)
                -- Otherwise, initialise both sets to empty
                newNode bb _ = Node (BlockNode bb) S.empty S.empty

        backwardEdgePass :: M.Map NodeID (Node (IR.BasicBlock r))
        -- Fold over the map - each element iterates over its outgoing node set, and
        -- for each node, adds its ID to their incoming node set
        backwardEdgePass = M.foldlWithKey linkEdges forwardEdgePass forwardEdgePass
            where
                -- For a given node with ID "nid", loop over every outgoing edge, and
                -- for each, add "nid" to its incoming edges
                linkEdges :: M.Map NodeID (Node (IR.BasicBlock r)) -> NodeID -> Node (IR.BasicBlock r) 
                          -> M.Map NodeID (Node (IR.BasicBlock r))
                linkEdges acc nid (Node _ is os) = foldl addEdge acc os
                    where
                        addEdge acc to = M.adjust (addIncomingEdge nid) to acc

        graph :: M.Map NodeID (Node (IR.BasicBlock r))
        graph = backwardEdgePass

