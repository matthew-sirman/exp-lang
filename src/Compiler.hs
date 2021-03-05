module Compiler (
    compile
) where

import Control.Monad.State
import qualified DataStructs.HashSet as S
import qualified DataStructs.HashMap as M
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import qualified SyntaxTree as ST
import Typing
import qualified IR

-- State uses while building a program's IR
data ProgState = ProgState
    { bstack :: [IR.BasicBlock IR.VarID]        -- Stack of basic blocks - we always edit the top
    , fstack :: [IR.Function IR.VarID]          -- Stack of functions - we always edit the top
    , prog :: IR.Program IR.VarID               -- The current program
    , labelID :: Int                            -- The current label ID - we need to generate these freshly
    , funcID :: Int                             -- The current function ID - these must also be fresh
    , varID :: IR.VarID                         -- The current temporary variable
    , hofMap :: M.HashMap IR.FuncID (IR.Closure IR.VarID)   -- a map of functions which return functions
    }

-- The variable context holds a finite mapping between identifiers
-- from the AST and variables in the IR
type VarContext = M.HashMap ST.Identifier (IR.Value IR.VarID)

-- The empty context, i.e. has no mapping
emptyCtx :: VarContext
emptyCtx = M.empty

-- Add a new value to the context (just equivalent to insertion)
ctxAdd :: ST.Identifier -> IR.Value IR.VarID -> VarContext -> VarContext
ctxAdd = M.insert

-- Lookup a value in the context
-- This should never be able to fail - the program must be closed to
-- have reached the compile phase. If this fails, there is a bug in 
-- the compiler!
ctxLookup :: ST.Identifier -> VarContext -> (IR.Value IR.VarID)
ctxLookup name ctx = 
    case M.lookup name ctx of
        Just v -> v
        Nothing -> error $ "DEV: Tried to lookup missing element in context."

-- Data structure for typed expressions with builtins reconstructed
-- This makes compilation easier - we want operators to be computable
-- as binary functions, rather than as partially applicable functions
data Expr
    = App Type Expr Expr
    | Lam Type ST.Pattern Expr
    | Let Type ST.Pattern Expr Expr
    | Bin Type ST.Builtin Expr Expr
    | IfThenElse Expr Expr Expr
    | Lit ST.Literal
    | Pair Expr Expr
    | Var Type ST.Identifier
    deriving Show

-- Helper function for reconstructing the AST's builtins
reconstructBuiltins :: ST.Expr Type -> Expr
reconstructBuiltins = reconstruct
    where
        -- This is the only interesting case, where we match on nested applications
        -- with a builtin operator (note this is the inverse of how we constructed
        -- these in SyntaxTree)
        reconstruct (ST.App _ (ST.App _ (ST.Var _ (ST.Builtin op)) lhs) rhs) = 
            Bin (t op) op (reconstruct lhs) (reconstruct rhs)
            where
                t (ST.BinOp _) = IntTy 
                t (ST.CmpOp _) = BoolTy
        reconstruct (ST.App t e0 e1) = App t (reconstruct e0) (reconstruct e1)
        reconstruct (ST.Lam t x e) = Lam t x (reconstruct e)
        reconstruct (ST.Let t x e e') = Let t x (reconstruct e) (reconstruct e')
        reconstruct (ST.IfThenElse p c a) = IfThenElse (reconstruct p) (reconstruct c) (reconstruct a)
        reconstruct (ST.Lit l) = Lit l
        reconstruct (ST.Pair l r) = Pair (reconstruct l) (reconstruct r)
        reconstruct (ST.Var t (ST.Identifier i)) = Var t i
        -- This shouldn't be able to happen in the current model - this would apply partial
        -- application of an operator, which should have been rejected by the parser.
        reconstruct (ST.Var _ (ST.Builtin i)) = error $ "DEV: Failed to reconstruct binary operator '" ++ show i ++ "'."

-- Helper function to find the size of a type
sizeof :: Type -> Int
sizeof = sizeof'
    where
        sizeof' UnitTy = 0
        sizeof' IntTy = intSize
        sizeof' BoolTy = 1
        sizeof' (PairTy _ _) = ptrSize
        sizeof' (FuncTy _ _) = ptrSize
        sizeof' (PolyTy _) = ptrSize

        intSize = 8
        ptrSize = 8

-- This is safe only because we have already type checked
-- The extra type information that wasn't strictly necessary, but
-- we tagged anyway, lets this operation be computable top down
-- without the need for a carrying context. Note this is only
-- valid if the expression is closed and type checked, but that
-- is a requirement before compiling, so this is never an issue.
typeof :: Expr -> Type
typeof (App t _ _) = t
typeof (Lam t _ e) = FuncTy t (typeof e)
typeof (Let _ _ _ e) = typeof e
typeof (Bin t _ _ _) = t
typeof (IfThenElse _ e _) = typeof e
typeof (Lit l) = lty l
    where
        lty (ST.UnitLit) = UnitTy
        lty (ST.IntLit _) = IntTy
        lty (ST.BoolLit _) = BoolTy
typeof (Pair l r) = PairTy (typeof l) (typeof r)
typeof (Var t _) = t

-- Helper function to find the set of free variables in an expression
findFVs :: Expr -> S.HashSet (ST.Identifier, Type)
findFVs = findFVs' S.empty
    where
        findFVs' ex (App _ e1 e2) = findFVs' ex e1 `S.union` findFVs' ex e2
        findFVs' ex (Lam _ pat e) = findFVs' (ex `S.union` patternVars pat) e
        findFVs' ex (Let _ pat e1 e2) = findFVs' ex e1 `S.union` (findFVs' (ex `S.union` patternVars pat) e2)
        findFVs' ex (Bin _ _ l r) = findFVs' ex l `S.union` findFVs' ex r
        findFVs' ex (IfThenElse p c a) = findFVs' ex p `S.union` findFVs' ex c `S.union` findFVs' ex a
        findFVs' ex (Lit _) = S.empty
        findFVs' ex (Pair l r) = findFVs' ex l `S.union` findFVs' ex r
        -- Only add to the free variable set if not bound in exlusion set
        findFVs' ex (Var t x) 
            | x `S.member` ex = S.empty
            | otherwise = S.singleton (x, t)

-- Helper function to find the set of all names in a pattern
patternVars :: ST.Pattern -> S.HashSet ST.Identifier
patternVars (ST.PVar x) = S.singleton x
patternVars (ST.PPair l r) = patternVars l `S.union` patternVars r
patternVars (ST.PLit _) = S.empty

-- Main compiler function. Takes a typed expression tree and produces a program
-- with an infinite register model
compile :: ST.Expr Type -> IR.Program IR.VarID
compile = mkProgram . reconstructBuiltins
    where
        -- Make the program from a tree with builtins reconstructed
        mkProgram :: Expr -> IR.Program IR.VarID
        mkProgram expr = evalState finalise startState
            where
                -- This wraps the top level main function in the program -
                -- we need an entry point and elements on the stack to add
                -- instructions to.
                finalise :: State ProgState (IR.Program IR.VarID)
                finalise = do
                    -- Make the main function
                    mkNamedFunc "emain" 0
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
        codegen :: VarContext -> Expr -> State ProgState (IR.Value IR.VarID)

        -- Generate an application
        codegen ctx (App _ f a) = do
            -- Generate the function itself
            -- Note we have already typechecked, so this
            -- should always return a function
            fval <- codegen ctx f
            -- Generate the argument value
            arg <- codegen ctx a
            -- Create a new variable to assign this call to
            call <- mkNewVar
            -- Apply the function to the argument
            addInstruction $ IR.Call call (fName fval) (argList fval arg)
            -- Return the result of this call
            cr <- gets $ callReturn call fval
            pure cr

            where
                -- NOTE: These should both be safe because the expression was type checked
                -- therefore the fval should ALWAYS return a function
                argList :: IR.Value IR.VarID -> IR.Value IR.VarID -> [IR.Value IR.VarID]
                -- If this is a direct call (i.e. not a closure), we only
                -- need a single argument
                argList (IR.Closure (IR.FClosure _ Nothing)) arg = [arg]
                -- Otherwise, attach the closure as the SECOND argument
                argList f@(IR.Closure (IR.FClosure _ (Just c))) arg = [arg, IR.Variable c]

                fName :: IR.Value IR.VarID -> IR.FuncID
                fName (IR.Closure (IR.FClosure f _)) = f

                callReturn :: IR.VarID -> IR.Value IR.VarID -> ProgState -> IR.Value IR.VarID
                callReturn callVar (IR.Closure (IR.FClosure f _)) p =
                    case M.lookup f (hofMap p) of
                        -- If the function wasn't higher order, just propagate the return
                        Nothing -> IR.Variable callVar
                        -- This is quite subtle - we are saying if the function we
                        -- are calling was in fact higher order (i.e. returned another
                        -- function closure) then we want to return a closure from this
                        -- call. This allows for multiple applications.
                        -- Now, if the function had no closure, this implies that it
                        -- did not need the variable(s) it bound to be captured. This tells
                        -- us that in fact we don't need a closure. This is the case where "cl"
                        -- is Nothing, and as such fmapping won't do anything. In the case
                        -- that there was a closure, cl will be Just, and we map const callVar
                        -- under it, thus replacing the value with callVar - which is where
                        -- the closure is now stored. This is because we know calling the function
                        -- returned a closure, and "callVar" is the value we save the return in.
                        Just cl -> IR.Closure (const callVar <$> cl)

        -- Generate a lambda abstraction
        codegen ctx lam@(Lam t pattern body) = do
            -- Create a new function in the program with one argument if
            -- there are no free variables, or 2 if there are (the second
            -- being for the closure)
            mkNewFunc (if requiresClosure then 2 else 1)
            -- Get a reference to the top function
            ref <- funcRef
            -- Create an entry block for this function
            mkNewBlock
            bodyCtx <- unpackPattern ctx pattern t (IR.Argument 0)
            -- Unpack the closure if necessary
            bodyCtx' <- unpackClosure (IR.Argument 1) bodyCtx
            -- Generate the body with the new names bound
            rval <- codegen bodyCtx' body
            -- Add the return instruction for the value
            -- generated in the body
            addInstruction $ IR.Ret rval
            -- Apply the top block on the stack - the body should leave
            -- the same number of blocks on the stack
            lamBlk <- popBlock
            applyBlock lamBlk
            -- Apply the function to the program
            applyFunc

            -- If this is a higher order function, add an entry to the higher
            -- order function map, saying this function returns another
            -- function.
            modify $ addHigherOrder rval ref

            -- Make a closure if necessary, otherwise return Nothing
            cl <- mkClosure
            pure $ IR.Closure (IR.FClosure ref cl)

            where
                addHigherOrder :: IR.Value IR.VarID -> IR.FuncID -> ProgState -> ProgState
                -- If the return value of the body was a closure, add an entry
                addHigherOrder (IR.Closure cl) f p = p { hofMap = M.insert f cl (hofMap p) }
                -- Otherwise don't change the state
                addHigherOrder _ _ p = p

                fvs :: [(ST.Identifier, Type)]
                fvs = toList $ findFVs lam

                requiresClosure :: Bool
                requiresClosure = case fvs of
                    [] -> False
                    _ -> True

                -- CLOSURE MEMORY LAYOUT
                -- We will make a closure of n items where each item
                -- is layed out sequentially in memory. The ordering is arbitrary
                -- but fixed by the "fvs" calculation (above). Item 0 will have
                -- an offset of 0 from the closure pointer

                unpackClosure :: IR.Value IR.VarID -> VarContext -> State ProgState VarContext
                unpackClosure ptr
                    | requiresClosure = unpackClosure' fvs ptr
                    | otherwise = pure
        
                -- Helper to unpack a closure and update the variable context
                -- This should only be called when there are more than one free variables
                unpackClosure' :: [(ST.Identifier, Type)] -> IR.Value IR.VarID 
                              -> VarContext -> State ProgState VarContext
                unpackClosure' ((i, t):[]) ptr ctx = do
                    -- create a fresh variable
                    var <- mkNewVar
                    -- read into this variable
                    addInstruction $ IR.Read var ptr (sizeof t)
                    -- return the context, with this variable added
                    pure $ M.insert i (IR.Variable var) ctx
                unpackClosure' ((i, t):is) ptr ctx = do
                    -- create a fresh variable
                    var <- mkNewVar
                    -- read into this variable
                    addInstruction $ IR.Read var ptr sz
                    -- update the pointer
                    ptr' <- mkNewVar
                    addInstruction $ IR.Add ptr' ptr (IR.Immediate $ IR.Int64 sz)
                    -- recurse
                    unpackClosure' is (IR.Variable ptr') (M.insert i (IR.Variable var) ctx)
                    where
                        sz = sizeof t

                -- Make a closure object on the heap
                mkClosure :: State ProgState (Maybe IR.VarID)
                mkClosure
                    | requiresClosure = Just <$> mkClosure'
                    | otherwise = pure Nothing

                -- Helper to make a closure. This should only be called
                -- if a closure is needed.
                mkClosure' :: State ProgState IR.VarID
                mkClosure' = do
                    -- Create a new var for the closure
                    closure <- mkNewVar
                    -- Allocate the right amount of heap memory
                    addInstruction $ IR.MAlloc closure (IR.Immediate $ IR.Int64 totalSize)
                    writeClosure fvs (IR.Variable closure)

                    -- Return the closure
                    pure closure
                    
                    where
                        totalSize = sum $ map (sizeof . snd) fvs

                        writeClosure :: [(ST.Identifier, Type)] -> IR.Value IR.VarID -> State ProgState ()
                        writeClosure ((i, t):[]) ptr = do
                            -- Lookup the name in the context (where it should ALWAYS be) and
                            -- write it to the memory location
                            addInstruction $ IR.Write (fromJust $ M.lookup i ctx) ptr (sizeof t)
                        writeClosure ((i, t):is) ptr = do
                            -- Do the same as above
                            addInstruction $ IR.Write (fromJust $ M.lookup i ctx) ptr sz
                            -- Increment the pointer
                            ptr' <- mkNewVar
                            addInstruction $ IR.Add ptr' ptr (IR.Immediate $ IR.Int64 sz)
                            -- Recurse
                            writeClosure is (IR.Variable ptr')
                            where
                                sz = sizeof t

        -- Generate a let binding
        codegen ctx (Let t pattern body use) = do
            -- Generate the body of the let
            -- Bind the given name to this body
            var <- codegen ctx body
            useCtx <- unpackPattern ctx pattern t var
            -- Generate the rest of the expression
            codegen useCtx use

        -- Generate an if then else
        codegen ctx (IfThenElse pred cons alt) = do
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
        codegen ctx (Bin _ op lhs rhs) = do
            lval <- codegen ctx lhs
            rval <- codegen ctx rhs
            var <- mkNewVar
            addInstruction $ opInstruction op var lval rval
            pure $ IR.Variable var

            where
                opInstruction (ST.BinOp ST.Add) = IR.Add
                opInstruction (ST.BinOp ST.Sub) = IR.Sub
                opInstruction (ST.BinOp ST.Mul) = IR.Mul
                opInstruction (ST.BinOp ST.Div) = IR.Div
                opInstruction (ST.CmpOp ST.EQ_) = IR.EQ
                opInstruction (ST.CmpOp ST.LT_) = IR.LT
                opInstruction (ST.CmpOp ST.GT_) = IR.GT
                opInstruction (ST.CmpOp ST.LE_) = IR.LE
                opInstruction (ST.CmpOp ST.GE_) = IR.GE

        -- Generate a literal value as an immediate
        codegen ctx (Lit l) = pure $ IR.Immediate $ imm l
            where
                imm (ST.UnitLit) = IR.Unit
                imm (ST.IntLit i) = IR.Int64 i
                imm (ST.BoolLit b) = IR.Bool b

        -- Generate a pair on the heap
        codegen ctx (Pair l r) = do
            -- Generate the left part of the pair
            lval <- codegen ctx l
            -- Generate the right part of the pair
            rval <- codegen ctx r
            -- Allocate an appropriate amount of heap memory
            pair <- mkNewVar
            addInstruction $ IR.MAlloc pair (IR.Immediate $ IR.Int64 (lSize + rSize))
            -- Get a pointer to the second element on the heap
            addr2 <- mkNewVar
            addInstruction $ IR.Add addr2 (IR.Variable pair) (IR.Immediate $ IR.Int64 lSize)
            -- Write out the two elements to the heap
            addInstruction $ IR.Write lval (IR.Variable pair) lSize
            addInstruction $ IR.Write rval (IR.Variable addr2) rSize
            -- Return the heap pointer to the pair
            pure $ IR.Variable pair

            where
                lSize = sizeof $ typeof l
                rSize = sizeof $ typeof r

        -- Generate a variable - this corresponds to looking
        -- it up in the context. This variable should
        -- always be present, otherwise the expression is not 
        -- closed, and therefore the program is invalid and would
        -- have failed the type checker.
        codegen ctx (Var _ name) = pure $ ctxLookup name ctx

        -- The inital state of the program. The function and block
        -- stacks start empty, as does the program. We initialise
        -- the three counters for tracking the current block/function/
        -- temporary to 0
        startState :: ProgState
        startState = ProgState [] [] (IR.Program M.empty) 0 0 0 M.empty

        -- Create a new block and push it onto the stack
        mkNewBlock :: State ProgState ()
        mkNewBlock = do
            modify addBlock
            where
                -- We call the blocks "block0", "block1", ...
                -- Each time we add a block, we also increment the label id
                addBlock ps@(ProgState bs _ _ lid _ _ _) = ps
                    { bstack = IR.mkBasicBlock ("block" ++ show lid) : (bstack ps)
                    , labelID = lid + 1
                    }

        -- Create a new function with an arbitrary name and push it onto the stack
        mkNamedFunc :: IR.FuncID -> Int -> State ProgState ()
        mkNamedFunc name as = do
            modify addFunc
            where
                -- Create the function based on the given name and push
                -- to the stack
                addFunc ps@(ProgState _ fs _ _ _ _ _) = ps
                    { fstack = IR.mkFunc name as : fstack ps
                    }

        -- Create a new function and push it onto the stack
        mkNewFunc :: Int -> State ProgState ()
        mkNewFunc as = do
            modify addFunc
            where
                -- We call the functions "func0", "func1", ...
                -- Each time we add one, we increment the function id
                addFunc ps@(ProgState _ fs _ _ fid _ _) = ps 
                    { fstack = IR.mkFunc ("func" ++ show fid) as : (fstack ps) 
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
                mk ps@(ProgState _ _ _ _ _ vid _) =
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
                pop ps@(ProgState (b:bs) _ _ _ _ _ _) =
                    (b, ps { bstack = bs })

        -- Add a block to the end of the current function
        applyBlock :: (IR.BasicBlock IR.VarID) -> State ProgState ()
        applyBlock blk = do
            modify appBlock
            where
                -- We peek at the top of the function stack and add this
                -- block to the end of it
                appBlock ps@(ProgState _ (f:fs) _ _ _ _ _) = ps
                    { fstack = (IR.pushBlock blk f) : fs
                    }

        -- Move the top function on the stack into the program
        applyFunc :: State ProgState ()
        applyFunc = do
            modify appFunc
            where
                -- We pop off the top function from the stack and add
                -- it to the program
                appFunc ps@(ProgState _ (f:fs) p _ _ _ _) = ps
                    { fstack = fs
                    , prog = IR.progAddFunc f p
                    }

        unpackPattern :: VarContext -> ST.Pattern -> Type -> IR.Value IR.VarID -> State ProgState VarContext
        -- If we are binding a name, add the binding from this name to the value
        -- unit type. No instructions need to be added for this
        unpackPattern ctx (ST.PVar name) t val = pure $ M.insert name val ctx
        -- If we are binding a pair, then we need to load heap values
        -- The value should be a heap address.
        unpackPattern ctx (ST.PPair l r) (PairTy tl tr) addr1 = do
            -- Make a new variable for whatever is loaded from the left
            -- part of the pair
            lvar <- mkNewVar
            -- Load from the address at "addr1" into lvar
            addInstruction $ IR.Read lvar addr1 lSize
            -- Now find the address of the second element in the pair
            addr2 <- mkNewVar
            addInstruction $ IR.Add addr2 addr1 (IR.Immediate $ IR.Int64 lSize)
            -- Make a new variable for whatever is loaded from the right part
            rvar <- mkNewVar
            -- Load from the address at "addr2" into rvar
            addInstruction $ IR.Read rvar (IR.Variable addr2) rSize

            -- Now recurse to unpack further nesting, making
            -- sure to feed the context through
            ctx' <- unpackPattern ctx l tl (IR.Variable lvar)
            unpackPattern ctx' r tr (IR.Variable rvar)

            where
                lSize = sizeof tl
                rSize = sizeof tr
        -- TODO: Maybe invoke some form of exception for this case if 
        -- the value doesn't equal the literal
        unpackPattern ctx (ST.PLit _) _ _ = pure ctx

        -- Get the name of the function on the top of the stack
        funcRef :: State ProgState IR.FuncID
        funcRef = do
            gets fn
            where
                fn (ProgState _ (f:_) _ _ _ _ _) = IR.fid f

        -- Get the label of the top block on the stack
        blockLabel :: State ProgState IR.Label
        blockLabel = do
            gets bl
            where
                bl (ProgState (b:_) _ _ _ _ _ _) = IR.label b

        -- Add an instruction to the end of the current block
        addInstruction :: IR.Instruction IR.VarID -> State ProgState ()
        addInstruction i = do
            modify addI
            where 
                -- Peek at the top block on the stack and push this 
                -- instruction to the end of it
                addI ps@(ProgState (b:bs) _ _ _ _ _ _) = ps
                    { bstack = IR.blockIPush i b : bs
                    }

