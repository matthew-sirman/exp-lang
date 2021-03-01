{
module Parser where

import Data.Char
}

%name parse Exp
%tokentype { Token }
%error { parseError }

%token
    let     { TokenLet }
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
    : let var '=' Exp in Exp    { Let $2 $4 $6 }
    | Exp Exp1                  { Application $1 $2 }
    | Exp1                      { Exp1 $1 }

Exp1
    : if Exp then Exp else Exp  { IfThenElse $2 $4 $6 }
    | '$' var '->' Exp          { Lambda $2 $4 }
    | AExp                      { ArithExp $1 }

AExp 
    : AExp '+' Term             { Plus $1 $3 }
    | AExp '-' Term             { Minus $1 $3 }
    | AExp '<' Term             { LessThan $1 $3 }
    | AExp '>' Term             { GreaterThan $1 $3 }
    | AExp '<=' Term            { LessEqual $1 $3 }
    | AExp '>=' Term            { GreaterEqual $1 $3 }
    | AExp '==' Term            { EqualTo $1 $3 }
    | Term                      { Term $1 }

Term 
    : Term '*' Factor           { Times $1 $3 }
    | Term '/' Factor           { Div $1 $3 }
    | Factor                    { Factor $1 }

Factor 
    : '()'                      { Unit }
    | int                       { Int $1 }
    | bool                      { Bool $1 }
    | var                       { Var $1 }
    | '(' Exp ',' Exp ')'       { Pair $2 $4 }
    | '(' Exp ')'               { Nested $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

type Identifier = String

data Exp
    = Let Identifier Exp Exp
    | Application Exp Exp1
    | Exp1 Exp1
    deriving Show

data Exp1
    = IfThenElse Exp Exp Exp
    | Lambda Identifier Exp
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

data Token
    = TokenLet
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

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexVar (c:cs)
    | isDigit c = lexNum (c:cs)
lexer ('(':')':cs) = TokenUnit : lexer cs
lexer ('=':'=':cs) = TokenEqEq : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('$':cs) = TokenLam : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('<':'=':cs) = TokenLE : lexer cs
lexer ('>':'=':cs) = TokenGE : lexer cs
lexer ('<':cs) = TokenLT : lexer cs
lexer ('>':cs) = TokenGT : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer (',':cs) = TokenComma : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
    where (num, rest) = span isDigit cs

lexVar cs =
    case span isAlpha cs of
        ("let"  , rest) -> TokenLet         : lexer rest
        ("in"   , rest) -> TokenIn          : lexer rest
        ("if"   , rest) -> TokenIf          : lexer rest
        ("then" , rest) -> TokenThen        : lexer rest
        ("else" , rest) -> TokenElse        : lexer rest
        ("True" , rest) -> TokenBool True   : lexer rest
        ("False", rest) -> TokenBool False  : lexer rest
        (var  , rest) -> TokenVar var       : lexer rest
}
