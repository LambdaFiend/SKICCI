{
module Lexer where
}

%wrapper "posn"

$white = [\ \t\n\r\b]
$lower = [a-z]

tokens :-

$white+ ;
S            { \pos _ -> Token pos SCOMB }
K            { \pos _ -> Token pos KCOMB }
I            { \pos _ -> Token pos ICOMB }
"("          { \pos _ -> Token pos LPAREN }
")"          { \pos _ -> Token pos RPAREN }
$lower+"\'"* { \pos s -> Token pos (ID s) }
.            { \pos s -> Token pos (ERROR ("Lexing error: " ++ s)) }

{
data Token = Token
  { tokenPos :: AlexPosn
  , tokenDat :: TokenData
  }
  deriving (Show, Eq)

data TokenData
  = SCOMB
  | KCOMB
  | ICOMB
  | LPAREN
  | RPAREN
  | ID String
  | ERROR String
  deriving (Show, Eq)
}
