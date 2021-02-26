module IR (
    module IR.Instruction
  , module IR.BasicBlock
  , module IR.Function
  , module IR.Program
  , module IR.FlowGraph
  , module IR.LVA
) where

-- Top level module for the IR. Imports the submodules
-- and exposes them

-- Import IR submodules
import IR.Instruction
import IR.BasicBlock
import IR.Function
import IR.Program
import IR.FlowGraph
import IR.LVA

