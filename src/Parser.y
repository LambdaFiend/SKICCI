{
module Parser where

import Lexer
import Syntax
}

%monad { Either String } { (>>=) } { return }
%name parser
%tokentype { Token }
%error { parseError }

%token

s   { Token pos SCOMB }
k   { Token pos KCOMB }
i   { Token pos ICOMB }
"(" { Token pos LPAREN }
")" { Token pos RPAREN }
id  { Token pos (ID s) }

%%

Term : App { $1 }

App
  : App Atom { TermNode (getFI $1) (TmApp $1 $2) }
  | Atom     { $1 }

Atom
  : s            { TermNode (tokenPos $1) TmSComb }
  | k            { TermNode (tokenPos $1) TmKComb }
  | i            { TermNode (tokenPos $1) TmIComb }
  | Var          { $1 }
  | "(" Term ")" { $2 }

Var : Name { TermNode (fst $1) (TmVar (snd $1)) }

Name : id { (tokenPos $1, (\(ID s) -> s) (tokenDat $1)) }

{
parseError :: [Token] -> Either String a
parseError [] = Left ("Parsing error near the end of the file")
parseError ((Token fi _):tokens) = Left ("Parsing error at:" ++ showFileInfoHappy fi)
parseError (x:xs) = Left "Parsing error"

showFileInfoHappy :: AlexPosn -> String
showFileInfoHappy (AlexPn p l c) =
  "\n" ++"Absolute Offset: " ++ show p ++ "\n"
  ++ "Line: " ++ show l ++ "\n"
  ++ "Column: " ++ show c
}
