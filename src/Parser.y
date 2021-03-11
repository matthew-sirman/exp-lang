{
module Parser (
    Exp(..)
  , ArgList(..)
  , Exp1(..)
  , AExp(..)
  , Term(..)
  , Factor(..)
  , Pat(..)
  , runLexer
  , parse
)where

import Data.Char
import Control.Monad.State
}

%name parse Exp
%tokentype { Token }
%error { parseError }

%token
    let     { TokenLet }
    rec     { TokenRec }
    in      { TokenIn }
    if      { TokenIf }
    then    { TokenThen }
    else    { TokenElse }
    bool    { TokenBool $$ }
    int     { TokenInt $$ }
    var     { TokenVar $$ }
    '()'    { TokenUnit }
    '='     { TokenEq }
    '$'     { TokenLam }
    '->'    { TokenArrow }
    '+'     { TokenPlus }
    '-'     { TokenMinus }
    '*'     { TokenTimes }
    '/'     { TokenDiv }
    '<'     { TokenLT }
    '>'     { TokenGT }
    '<='    { TokenLE }
    '>='    { TokenGE }
    '=='    { TokenEqEq }
    '('     { TokenOB }
    ')'     { TokenCB }
    ','     { TokenComma }
%%

Exp 
    : let Pat ArgList '=' Exp in Exp        { Let $2 $3 $5 $7 }
    | let rec var ArgList '=' Exp in Exp    { Rec $3 $4 $6 $8 }
    | Exp Exp1                              { Application $1 $2 }
    | Exp1                                  { Exp1 $1 }

ArgList
    :                                       { ArgEmpty }
    | ArgList Pat                           { ArgCons $2 $1 }

Exp1
    : if Exp then Exp else Exp              { IfThenElse $2 $4 $6 }
    | '$' Pat '->' Exp                      { Lambda $2 $4 }
    | AExp                                  { ArithExp $1 }

AExp 
    : AExp '+' Term                         { Plus $1 $3 }
    | AExp '-' Term                         { Minus $1 $3 }
    | AExp '<' Term                         { LessThan $1 $3 }
    | AExp '>' Term                         { GreaterThan $1 $3 }
    | AExp '<=' Term                        { LessEqual $1 $3 }
    | AExp '>=' Term                        { GreaterEqual $1 $3 }
    | AExp '==' Term                        { EqualTo $1 $3 }
    | Term                                  { Term $1 }

Term 
    : Term '*' Factor                       { Times $1 $3 }
    | Term '/' Factor                       { Div $1 $3 }
    | Factor                                { Factor $1 }

Factor 
    : '()'                                  { Unit }
    | int                                   { Int $1 }
    | bool                                  { Bool $1 }
    | var                                   { Var $1 }
    | '(' Exp ',' Exp ')'                   { Pair $2 $4 }
    | '(' Exp ')'                           { Nested $2 }

Pat
    : var                                   { PVar $1 }
    | '(' Pat ',' Pat ')'                   { PPair $2 $4 }
    | '()'                                  { PUnit }
    | int                                   { PInt $1 }
    | bool                                  { PBool $1 }
    | '(' Pat ')'                           { PNest $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

type Identifier = String

data Exp
    = Let Pat ArgList Exp Exp
    | Rec Identifier ArgList Exp Exp
    | Application Exp Exp1
    | Exp1 Exp1
    deriving Show

data ArgList
    = ArgEmpty
    | ArgCons Pat ArgList
    deriving Show

data Exp1
    = IfThenElse Exp Exp Exp
    | Lambda Pat Exp
    | ArithExp AExp
    deriving Show

data AExp
    = Plus AExp Term
    | Minus AExp Term
    | LessThan AExp Term
    | GreaterThan AExp Term
    | LessEqual AExp Term
    | GreaterEqual AExp Term
    | EqualTo AExp Term
    | Term Term
    deriving Show

data Term
    = Times Term Factor
    | Div Term Factor
    | Factor Factor
    deriving Show

data Factor
    = Unit
    | Int Int
    | Bool Bool
    | Var String
    | Pair Exp Exp
    | Nested Exp
    deriving Show

data Pat
    = PVar Identifier 
    | PPair Pat Pat 
    | PUnit 
    | PInt Int 
    | PBool Bool
    | PNest Pat
    deriving Show

data Token
    = TokenLet
    | TokenRec
    | TokenIn
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenBool Bool
    | TokenInt Int
    | TokenVar String
    | TokenUnit
    | TokenEq
    | TokenLam
    | TokenArrow
    | TokenPlus
    | TokenMinus
    | TokenTimes
    | TokenLT
    | TokenGT
    | TokenLE
    | TokenGE
    | TokenEqEq
    | TokenDiv
    | TokenOB
    | TokenCB
    | TokenComma
    deriving Show

data LexerState = LexerState
    { inComment :: Bool
    }

runLexer :: String -> [Token]
runLexer source = evalState (lexer source) emptyLexState
    where
        emptyLexState = LexerState
            { inComment = False
            }

lexer :: String -> State LexerState [Token]
lexer [] = pure []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexVar (c:cs)
    | isDigit c = lexNum (c:cs)
lexer ('(':'*':cs)  = do
    modify setComment
    lexer cs
    where
        setComment s = s { inComment = True }
lexer ('*':')':cs)  = do
    modify unsetComment
    lexer cs
    where
        unsetComment s = s { inComment = False }
lexer ('(':')':cs)  = add TokenUnit  cs
lexer ('=':'=':cs)  = add TokenEqEq  cs
lexer ('=':cs)      = add TokenEq    cs
lexer ('$':cs)      = add TokenLam   cs
lexer ('-':'>':cs)  = add TokenArrow cs
lexer ('+':cs)      = add TokenPlus  cs
lexer ('-':cs)      = add TokenMinus cs
lexer ('*':cs)      = add TokenTimes cs
lexer ('/':cs)      = add TokenDiv   cs
lexer ('<':'=':cs)  = add TokenLE    cs
lexer ('>':'=':cs)  = add TokenGE    cs
lexer ('<':cs)      = add TokenLT    cs
lexer ('>':cs)      = add TokenGT    cs
lexer ('(':cs)      = add TokenOB    cs
lexer (')':cs)      = add TokenCB    cs
lexer (',':cs)      = add TokenComma cs

lexNum cs = add (TokenInt (read num)) rest
    where (num, rest) = span isDigit cs

lexVar cs =
    case span isAlphaNum cs of
        ("let"  , rest) -> add TokenLet             rest
        ("rec"  , rest) -> add TokenRec             rest
        ("in"   , rest) -> add TokenIn              rest
        ("if"   , rest) -> add TokenIf              rest
        ("then" , rest) -> add TokenThen            rest
        ("else" , rest) -> add TokenElse            rest
        ("True" , rest) -> add (TokenBool True)     rest
        ("False", rest) -> add (TokenBool False)    rest
        (var    , rest) -> add (TokenVar var)       rest

add :: Token -> String -> State LexerState [Token]
add t cs = do
    comment <- gets inComment
    rest <- lexer cs
    if comment then
        pure rest
    else
        pure $ t : rest

}
