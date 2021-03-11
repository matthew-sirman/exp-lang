module Main where

import System.FilePath (replaceExtension)
import System.Process (callCommand)
import System.Environment
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Parser (parse, runLexer)
import SyntaxTree (convertToAST, Expr)
import Typing (inferTypeTree, TypeError, Type)
import Compiler (compile)
import Generators.X86_64 (generateX86)
import Asm.X86_64Printer
import IR (Program, VarID)

type Error = String

data Options = Options
    { source :: FilePath
    , runtime :: FilePath
    }

type Process a = ExceptT Error (StateT Options IO) a

emptyOptions :: Options
emptyOptions = Options "" ""

getOptions :: Process ()
getOptions = do
    args <- liftIO getArgs
    case args of
        (sf:rt:_) -> lift $ modify $ setSource sf . setRuntime rt
        _ -> throwE "Requires 2 arguments: <source> <runtime>"

    where
        setSource s opt = opt { source = s }
        setRuntime r opt = opt { runtime = r }

loadSource :: Process String
loadSource = do
    sourceFile <- lift $ gets source
    liftIO $ readFile sourceFile

process :: String -> Process (Expr Type)
process source =
    case proc source of
        Left err -> throwE err
        Right (expr, _) -> pure expr
    where
        proc = inferTypeTree . convertToAST . parse . runLexer

compileCode :: Expr Type -> Process (Program VarID)
compileCode = pure . compile

outputX86 :: Program VarID -> Process ()
outputX86 p = do
    destFile <- lift $ gets $ flip replaceExtension "s" . source
    liftIO $ writeFile destFile (show x86)
    where
        x86 = generateX86 p

buildExecutable :: Process ()
buildExecutable = do
    src <- lift $ gets source
    rt <- lift $ gets runtime
    let asm = replaceExtension src ".s"
        exe = replaceExtension src ""
    liftIO $ callCommand $ "gcc -o " ++ exe ++ " " ++ rt ++ " " ++ asm

pipeline :: Process ()
pipeline = do
    getOptions
    source <- loadSource
    expr <- process source
    prog <- compileCode expr
    outputX86 prog
    buildExecutable

main :: IO ()
main = do
    res <- evalStateT (runExceptT pipeline) emptyOptions
    case res of
        Left err -> putStrLn err
        Right () -> putStrLn "Success!"
    
