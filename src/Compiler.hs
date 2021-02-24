module Compiler where

import SyntaxTree
import Types
import Typing
import Builtin
import qualified IR
import Control.Monad.State
import qualified Data.Map as M

data ProgState = ProgState
    { bstack :: [IR.BasicBlock]
    , fstack :: [IR.Function]
    , prog :: IR.Program
    , labelID :: Int
    , funcID :: Int
    , varID :: IR.VarID
    }

type VarContext = M.Map Identifier IR.Value

emptyCtx :: VarContext
emptyCtx = M.empty

ctxAdd :: Identifier -> IR.Value -> VarContext -> VarContext
ctxAdd = M.insert

ctxLookup :: Identifier -> VarContext -> IR.Value
ctxLookup name ctx = 
    case M.lookup name ctx of
        Just v -> v
        Nothing -> error $ "DEV: Tried to lookup missing element in context."

data BTExpr
    = BTApp BTExpr BTExpr
    | BTLam Type Identifier BTExpr
    | BTLet Type Identifier BTExpr BTExpr
    | BTBin Builtin BTExpr BTExpr
    | BTITE BTExpr BTExpr BTExpr
    | BTLit Literal
    | BTVar Identifier

reconstructBuiltins :: TExpr -> BTExpr
reconstructBuiltins = reconstruct
    where
        reconstruct (TApp (TApp (TVar (Builtin op)) lhs) rhs) = BTBin op (reconstruct lhs) (reconstruct rhs)
        reconstruct (TApp e0 e1) = BTApp (reconstruct e0) (reconstruct e1)
        reconstruct (TLambda t x e) = BTLam t x (reconstruct e)
        reconstruct (TLet t x e e') = BTLet t x (reconstruct e) (reconstruct e')
        reconstruct (TIfThenElse p c a) = BTITE (reconstruct p) (reconstruct c) (reconstruct a)
        reconstruct (TLit l) = BTLit l
        reconstruct (TVar (Identifier i)) = BTVar i
        reconstruct (TVar (Builtin i)) = error $ "DEV: Failed to reconstruct binary operator '" ++ show i ++ "'."

compile :: TExpr -> IR.Program
compile = mkProgram . reconstructBuiltins
    where
        mkProgram :: BTExpr -> IR.Program
        mkProgram expr = p
            where
                finalise :: State ProgState ()
                finalise = do
                    mkNamedFunc "main"
                    mkNewBlock
                    res <- codegen emptyCtx expr
                    addInstruction $ IR.Ret res
                    finalBlock <- popBlock
                    applyBlock finalBlock
                    applyFunc

                (_, ps) = runState finalise startState
                p = prog ps

        codegen :: VarContext -> BTExpr -> State ProgState IR.Value

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

        codegen ctx (BTLet _ name body use) = do
            -- Generate the body of the let
            -- Bind the given name to this body
            var <- codegen ctx body
            -- Generate the rest of the expression
            codegen (ctxAdd name var ctx) use

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

        -- codegen ctx (TBinOp op lhs rhs) = do
        --     lval <- codegen ctx lhs
        --     rval <- codegen ctx rhs
        --     var <- mkNewVar
        --     addInstruction $ opInstruction op var lval rval
        --     pure $ IR.Variable var
        --     where
        --         opInstruction Add = IR.Add
        --         opInstruction Sub = IR.Sub 
        --         opInstruction Mul = IR.Mul 
        --         opInstruction Div = IR.Div

        -- codegen ctx (TCmpOp op lhs rhs) = do
        --     lval <- codegen ctx lhs
        --     rval <- codegen ctx rhs
        --     var <- mkNewVar
        --     addInstruction $ opInstruction op var lval rval
        --     pure $ IR.Variable var
        --     where
        --         opInstruction EQ_ = IR.EQ
        --         opInstruction LT_ = IR.LT
        --         opInstruction GT_ = IR.GT
        --         opInstruction LE_ = IR.LE
        --         opInstruction GE_ = IR.GE

        -- codegen ctx (TIntLit i) = pure $ IR.Immediate $ IR.Int64 i

        -- codegen ctx (TBoolLit b) = pure $ IR.Immediate $ IR.Bool b

        codegen ctx (BTLit l) = pure $ IR.Immediate $ imm l
            where
                imm (IntLit i) = IR.Int64 i
                imm (BoolLit b) = IR.Bool b

        codegen ctx (BTVar name) = pure $ ctxLookup name ctx

        -- emptyMain :: IR.BasicBlock
        -- emptyMain = IR.mkBasicBlock 0

        -- mainFunc :: IR.Function
        -- mainFunc = IR.pushBlock emptyMain $ IR.mkFunc 0

        startState :: ProgState
        startState = ProgState [] [] defaultProgram 0 0 0
            where
                defaultProgram = IR.Program M.empty

        -- Create a new block and push it onto the stack
        mkNewBlock :: State ProgState ()
        mkNewBlock = do
            modify addBlock
            where
                addBlock ps@(ProgState bs _ _ lid _ _) = ps
                    { bstack = IR.mkBasicBlock ("block" ++ show lid) : (bstack ps)
                    , labelID = lid + 1
                    }

        -- Create a new function with an arbitrary name and push it onto the stack
        mkNamedFunc :: IR.FuncID -> State ProgState ()
        mkNamedFunc name = do
            modify addFunc
            where
                addFunc ps@(ProgState _ fs _ _ _ _) = ps
                    { fstack = IR.mkFunc name : fstack ps
                    }

        -- Create a new function and push it onto the stack
        mkNewFunc :: State ProgState ()
        mkNewFunc = do
            modify addFunc
            where
                addFunc ps@(ProgState _ fs _ _ fid _) = ps 
                    { fstack = IR.mkFunc ("func" ++ show fid) : (fstack ps) 
                    , funcID = fid + 1
                    }

        mkNewVar :: State ProgState IR.VarID
        mkNewVar = do
            (vid, ps) <- gets mk
            put ps
            pure vid
            where
                mk ps@(ProgState _ _ _ _ _ vid) =
                    (vid, ps { varID = vid + 1 })

        -- Pop the top block off the stack and return it
        popBlock :: State ProgState IR.BasicBlock
        popBlock = do
            (b, ps) <- gets pop
            put ps
            pure b
            where
                pop ps@(ProgState (b:bs) _ _ _ _ _) =
                    (b, ps { bstack = bs })

        -- Add a block to the end of the current function
        applyBlock :: IR.BasicBlock -> State ProgState ()
        applyBlock blk = do
            modify appBlock
            where
                appBlock ps@(ProgState _ (f:fs) _ _ _ _) = ps
                    { fstack = (IR.pushBlock blk f) : fs
                    }

        -- Move the top function on the stack into the program
        applyFunc :: State ProgState ()
        applyFunc = do
            modify appFunc
            where
                appFunc ps@(ProgState _ (f:fs) p _ _ _) = ps
                    { fstack = fs
                    , prog = IR.progAddFunc f p
                    }

        funcRef :: State ProgState IR.FuncID
        funcRef = do
            gets fn
            where
                fn (ProgState _ (f:_) _ _ _ _) = IR.fid f

        blockLabel :: State ProgState IR.Label
        blockLabel = do
            gets bl
            where
                bl (ProgState (b:_) _ _ _ _ _) = IR.label b

        -- Add an instruction to the end of the current block
        addInstruction :: IR.Instruction -> State ProgState ()
        addInstruction i = do
            modify addI
            where 
                addI ps@(ProgState (b:bs) _ _ _ _ _) = ps
                    { bstack = IR.blockIPush i b : bs
                    }

