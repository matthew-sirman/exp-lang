module InstructionGraph where

import Control.Monad.State
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Sequence as Seq
import Data.Foldable (toList)
import qualified IR

newtype InstructionGraph = InstructionGraph
    { edges :: [(IR.Instruction, IR.Instruction)]
    }
    
bbToGraph :: IR.BasicBlock -> InstructionGraph
bbToGraph (BasicBlock lab is) = undefined

updateInstructions :: InstructionGraph -> IR.BasicBlock -> IR.BasicBlock
updateInstructions (InstructionGraph es) (BasicBlock lab _) = BasicBlock lab is
    where
        is = evalState (graphToISeq neighbourSetModel) $ toList startSet
        
        -- An instruction graph in a basic block should always be a DAG -
        -- instructions cannot reference any earlier instructions, as there
        -- are no loops in a BB. We can then turn this graph back into a sequence
        -- with a topological sort
        graphToISeq :: M.Map IR.Instruction (S.Set IR.Instruction) 
                    -> State [IR.Instruction] (Seq IR.Instruction)
        graphToISeq graph = do
            s <- get
            if null s then
            n <- popNode

        popNode :: State [IR.Instruction] IR.Instruction
        popNode = do
            s <- get
            case s of
                (n:ns) -> do
                    put ns
                    pure n
                _ -> error "DEV ERROR: Non DAG found in toposort!"

        startSet :: [(IR.Instruction, IR.Instruction)] -> S.Set IR.Instruction
        startSet = ss es S.empty
            where
                ss [] acc = acc
                -- On the pass down, add each node which is the start of an edge
                -- On the pass back up, delete each node which appears at the end
                -- of an edge.
                -- This returns the set of each nodes with no incoming edges
                ss ((a, b):rest) acc = S.delete b (ss rest (S.insert a acc))

        neighbourSetModel :: M.Map IR.Instruction (S.Set IR.Instruction)
        neighbourSetModel = nsm M.empty es
            where
                nsm acc [] = acc
                nsm acc ((a, b):rest) = nsm (M.insert a $ M.findWithDefault S.empty a acc) rest
                    where
                        newAcc = M.insert a $ S.insert b (M.findWithDefault S.empty a acc)

