module Compiler (
    compile
) where

import SyntaxTree
import Typing
import qualified IR
import Control.Monad.State
import qualified DataStructs.HashMap as M

-- State uses while building a program's IR
data ProgState = ProgState
    { bstack :: [IR.BasicBlock IR.VarID]    -- Stack of basic blocks - we always edit the top
    , fstack :: [IR.Function IR.VarID]      -- Stack of functions - we always edit the top
    , prog :: IR.Program IR.VarID           -- The current program
    , labelID :: Int                        -- The current label ID - we need to generate these freshly
    , funcID :: Int                         -- The current function ID - these must also be fresh
    , varID :: IR.VarID                     -- The current temporary variable
    }

-- The variable context holds a finite mapping between identifiers
-- from the AST and variables in the IR
type VarContext = M.HashMap Identifier (IR.Value IR.VarID)

-- The empty context, i.e. has no mapping
emptyCtx :: VarContext
emptyCtx = M.empty

-- Add a new value to the context (just equivalent to insertion)
ctxAdd :: Identifier -> IR.Value IR.VarID -> VarContext -> VarContext
ctxAdd = M.insert

-- Lookup a value in the context
-- This should never be able to fail - the program must be closed to
-- have reached the compile phase. If this fails, there is a bug in 
-- the compiler!
ctxLookup :: Identifier -> VarContext -> (IR.Value IR.VarID)
ctxLookup name ctx = 
    case M.lookup name ctx of
        Just v -> v
        Nothing -> error $ "DEV: Tried to lookup missing element in context."

-- Data structure for typed expressions with builtins reconstructed
-- This makes compilation easier - we want operators to be computable
-- as binary functions, rather than as partially applicable functions
data BTExpr
    = BTApp BTExpr BTExpr
    | BTLam Type Identifier BTExpr
    | BTLet Type Identifier BTExpr BTExpr
    | BTBin Builtin BTExpr BTExpr
    | BTITE BTExpr BTExpr BTExpr
    | BTLit Literal
    | BTVar Identifier

-- Helper function for reconstructing the AST's builtins
reconstructBuiltins :: TExpr -> BTExpr
reconstructBuiltins = reconstruct
    where
        -- This is the only interesting case, where we match on nested applications
        -- with a builtin operator (note this is the inverse of how we constructed
        -- these in SyntaxTree)
        reconstruct (TApp (TApp (TVar (Builtin op)) lhs) rhs) = BTBin op (reconstruct lhs) (reconstruct rhs)
        reconstruct (TApp e0 e1) = BTApp (reconstruct e0) (reconstruct e1)
        reconstruct (TLambda t x e) = BTLam t x (reconstruct e)
        reconstruct (TLet t x e e') = BTLet t x (reconstruct e) (reconstruct e')
        reconstruct (TIfThenElse p c a) = BTITE (reconstruct p) (reconstruct c) (reconstruct a)
        reconstruct (TLit l) = BTLit l
        reconstruct (TVar (Identifier i)) = BTVar i
        -- This shouldn't be able to happen in the current model - this would apply partial
        -- application of an operator, which should have been rejected by the parser.
        reconstruct (TVar (Builtin i)) = error $ "DEV: Failed to reconstruct binary operator '" ++ show i ++ "'."

-- Main compiler function. Takes a typed expression tree and produces a program
-- with an infinite register model
compile :: TExpr -> IR.Program IR.VarID
compile = mkProgram . reconstructBuiltins
    where
        -- Make the program from a tree with builtins reconstructed
        mkProgram :: BTExpr -> IR.Program IR.VarID
        mkProgram expr = evalState finalise startState
            where
                -- This wraps the top level main function in the program -
                -- we need an entry point and elements on the stack to add
                -- instructions to.
                finalise :: State ProgState (IR.Program IR.VarID)
                finalise = do
                    -- Make the main function
                    mkNamedFunc "main"
                    -- Add the entry block
                    mkNewBlock
                    -- Compile the program - this will return a value
                    res <- codegen emptyCtx expr
                    -- Add a final return instruction from the main
                    -- function
                    addInstruction $ IR.Ret res
                    -- Apply the block to the function, and then the function
                    -- to the program
                    finalBlock <- popBlock
                    applyBlock finalBlock
                    applyFunc

                    -- We actually care in the end about the program itself
                    -- so return this from the monad
                    gets prog

        -- Generate the code for an expression.
        -- This is the main driver for the compiler. It takes a context
        -- mapping identifiers to values, and an expression to compile
        -- It returns in the monad the value computed by this expression.
        -- The function maintains the invariant that the number of blocks
        -- and functions on the stack is unchanged during a call - this
        -- way we prove that we will never try to pop a block or function which
        -- is not present on the stack.
        codegen :: VarContext -> BTExpr -> State ProgState (IR.Value IR.VarID)

        -- Generate an application
        codegen ctx (BTApp f a) = do
            -- Generate the function itself
            -- Note we have already typechecked, so this
            -- should always return a function
            -- NOTE: This disallows functions as arguments!
            fval <- codegen ctx f
            -- Generate the argument value
            arg <- codegen ctx a
            -- Create a new variable to assign this call to
            call <- mkNewVar
            -- Apply the function to the argument
            addInstruction $ IR.Call call fval arg
            -- Return the variable of this call
            pure $ IR.Variable call

        -- Generate a lambda abstraction
        codegen ctx lam@(BTLam _ name body) = do
            -- Create a new function in the program
            mkNewFunc
            -- Get a reference to the top function
            ref <- funcRef
            -- Create an entry block for this function
            mkNewBlock
            -- Generate the body with the name bound to the argument
            rval <- codegen (ctxAdd name IR.Argument ctx) body
            -- Add the return instruction for the value
            -- generated in the body
            addInstruction $ IR.Ret rval
            -- Apply the top block on the stack - the body should leave
            -- the same number of blocks on the stack
            lamBlk <- popBlock
            applyBlock lamBlk
            -- Apply the function to the program
            applyFunc

            -- Return the reference to this function
            pure $ IR.UserFunc ref

        -- Generate a let binding
        codegen ctx (BTLet _ name body use) = do
            -- Generate the body of the let
            -- Bind the given name to this body
            var <- codegen ctx body
            -- Generate the rest of the expression
            codegen (ctxAdd name var ctx) use

        -- Generate an if then else
        codegen ctx (BTITE pred cons alt) = do
            -- Generate the predicate in this block
            pval <- codegen ctx pred

            -- Next, we generate the the alt branch and get its
            -- label and return value
            mkNewBlock
            ablk <- blockLabel
            aval <- codegen ctx alt
            -- Pop the alt block
            altBlock <- popBlock

            -- Add a conditional branch to the alt block to the original block
            -- conditioned on pval
            addInstruction $ IR.Branch pval (IR.label altBlock)
            -- Now we can pop the entry block and apply it
            entryBlock <- popBlock
            applyBlock entryBlock

            -- Now generate the cons branch in a new basic block and get a reference to its label
            mkNewBlock
            cblk <- blockLabel
            cval <- codegen ctx cons
            consBlock <- popBlock

            -- Next generate the rest block (keeping the invariant that the number
            -- of blocks doesn't change in a call)
            mkNewBlock
            rblk <- blockLabel

            -- First push the cons block, with one extra instruction at the end
            -- which is an unconditional jump to the rest block
            applyBlock $ IR.blockIPush (IR.Jump rblk) consBlock
            -- Then push the alternate block
            applyBlock altBlock

            -- Finally, we coalesce the two branches using a phi instruction
            -- and return this as the value of this expression
            phi <- mkNewVar
            addInstruction $ IR.Phi phi (cval, cblk) (aval, ablk)

            pure $ IR.Variable phi

            -- The order should now be:
            --
            --  entry:
            --      ... leading code
            --      predicate calculation
            --      branch if false: alt
            --  cons:
            --      consequent calculation
            --      jump: rest
            --  alt:
            --      alt calculation
            --  rest:
            --      phi coalescence
            --      rest of code...

        -- Generate a binary operator for which there is an instruction
        -- in the IR
        codegen ctx (BTBin op lhs rhs) = do
            lval <- codegen ctx lhs
            rval <- codegen ctx rhs
            var <- mkNewVar
            addInstruction $ opInstruction op var lval rval
            pure $ IR.Variable var

            where
                opInstruction (BinOp Add) = IR.Add
                opInstruction (BinOp Sub) = IR.Sub
                opInstruction (BinOp Mul) = IR.Mul
                opInstruction (BinOp Div) = IR.Div
                opInstruction (CmpOp EQ_) = IR.EQ
                opInstruction (CmpOp LT_) = IR.LT
                opInstruction (CmpOp GT_) = IR.GT
                opInstruction (CmpOp LE_) = IR.LE
                opInstruction (CmpOp GE_) = IR.GE

        -- Generate a literal value as an immediate
        codegen ctx (BTLit l) = pure $ IR.Immediate $ imm l
            where
                imm (IntLit i) = IR.Int64 i
                imm (BoolLit b) = IR.Bool b

        -- Generate a variable - this corresponds to looking
        -- it up in the context. This variable should
        -- always be present, otherwise the expression is not 
        -- closed, and therefore the program is invalid and would
        -- have failed the type checker.
        codegen ctx (BTVar name) = pure $ ctxLookup name ctx

        -- The inital state of the program. The function and block
        -- stacks start empty, as does the program. We initialise
        -- the three counters for tracking the current block/function/
        -- temporary to 0
        startState :: ProgState
        startState = ProgState [] [] (IR.Program M.empty) 0 0 0

        -- Create a new block and push it onto the stack
        mkNewBlock :: State ProgState ()
        mkNewBlock = do
            modify addBlock
            where
                -- We call the blocks "block0", "block1", ...
                -- Each time we add a block, we also increment the label id
                addBlock ps@(ProgState bs _ _ lid _ _) = ps
                    { bstack = IR.mkBasicBlock ("block" ++ show lid) : (bstack ps)
                    , labelID = lid + 1
                    }

        -- Create a new function with an arbitrary name and push it onto the stack
        mkNamedFunc :: IR.FuncID -> State ProgState ()
        mkNamedFunc name = do
            modify addFunc
            where
                -- Create the function based on the given name and push
                -- to the stack
                addFunc ps@(ProgState _ fs _ _ _ _) = ps
                    { fstack = IR.mkFunc name : fstack ps
                    }

        -- Create a new function and push it onto the stack
        mkNewFunc :: State ProgState ()
        mkNewFunc = do
            modify addFunc
            where
                -- We call the functions "func0", "func1", ...
                -- Each time we add one, we increment the function id
                addFunc ps@(ProgState _ fs _ _ fid _) = ps 
                    { fstack = IR.mkFunc ("func" ++ show fid) : (fstack ps) 
                    , funcID = fid + 1
                    }

        -- Create a new temporary variable. The initial register model
        -- allows us to create an unbounded number of these. We also
        -- with to create a program is SSA form, so these should be
        -- readily created.
        mkNewVar :: State ProgState IR.VarID
        mkNewVar = do
            -- Get the top variable and updated state
            (vid, ps) <- gets mk
            -- Update the state
            put ps
            -- Return the fresh variable
            pure vid
            where
                mk ps@(ProgState _ _ _ _ _ vid) =
                    (vid, ps { varID = vid + 1 })

        -- Pop the top block off the stack and return it
        popBlock :: State ProgState (IR.BasicBlock IR.VarID)
        popBlock = do
            -- Get the top block and updated state
            (b, ps) <- gets pop
            -- Update the state
            put ps
            -- Return the block
            pure b
            where
                pop ps@(ProgState (b:bs) _ _ _ _ _) =
                    (b, ps { bstack = bs })

        -- Add a block to the end of the current function
        applyBlock :: (IR.BasicBlock IR.VarID) -> State ProgState ()
        applyBlock blk = do
            modify appBlock
            where
                -- We peek at the top of the function stack and add this
                -- block to the end of it
                appBlock ps@(ProgState _ (f:fs) _ _ _ _) = ps
                    { fstack = (IR.pushBlock blk f) : fs
                    }

        -- Move the top function on the stack into the program
        applyFunc :: State ProgState ()
        applyFunc = do
            modify appFunc
            where
                -- We pop off the top function from the stack and add
                -- it to the program
                appFunc ps@(ProgState _ (f:fs) p _ _ _) = ps
                    { fstack = fs
                    , prog = IR.progAddFunc f p
                    }

        -- Get the name of the function on the top of the stack
        funcRef :: State ProgState IR.FuncID
        funcRef = do
            gets fn
            where
                fn (ProgState _ (f:_) _ _ _ _) = IR.fid f

        -- Get the label of the top block on the stack
        blockLabel :: State ProgState IR.Label
        blockLabel = do
            gets bl
            where
                bl (ProgState (b:_) _ _ _ _ _) = IR.label b

        -- Add an instruction to the end of the current block
        addInstruction :: IR.Instruction IR.VarID -> State ProgState ()
        addInstruction i = do
            modify addI
            where 
                -- Peek at the top block on the stack and push this 
                -- instruction to the end of it
                addI ps@(ProgState (b:bs) _ _ _ _ _) = ps
                    { bstack = IR.blockIPush i b : bs
                    }

