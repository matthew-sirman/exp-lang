module Main where

import System.IO
import System.Environment

import Parser

main :: IO ()
main = do
    nArgs <- length <$> getArgs
    if nArgs < 1 then
        error "Requires source file argument."
    else do
        file <- head <$> getArgs
        source <- readFile file
        print $ calc $ lexer source
    
