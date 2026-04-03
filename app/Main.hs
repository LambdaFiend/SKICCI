module Main where

import           Data.Char                (isDigit)
import           Data.List                (intercalate, nub)
import           Evaluation
import           Lexer
import           Parser
import           Syntax
import           System.Console.Haskeline

type Environment = [(String, TermNode)]

commandList :: String
commandList =
  "-----------------------------------------------------------------------------------\n"
    ++ "| The default reduction order is leftmost                                         |\n"
    ++ "| :?, :h, :help             -> for help                                           |\n"
    ++ "| :q, :quit                 -> for closing the program                            |\n"
    ++ "| <ski-term>                -> shows the term and then evaluates it               |\n"
    ++ "| :left                     -> for using left-most order reduction                |\n"
    ++ "| :right                    -> for using right-most order reduction               |\n"
    ++ "| :let <var> = <ski-term>   -> assigns a ski-term to an environtment variable     |\n"
    ++ "| :s, :show <var>           -> shows the contents of a variable                   |\n"
    ++ "| :e, :eval <var>           -> evaluates a variable up-to normal form (in-place)  |\n"
    ++ "| :e, :eval <number> <var>  -> evaluates a variable n times (in-place)            |\n"
    ++ "| :eq, :equal <var1> <var2> -> checks if var1 and var2 are sintactically the same |\n"
    ++ "| :showenv                  -> shows the list of names of environment variables   |\n"
    ++ "-----------------------------------------------------------------------------------"

main :: IO ()
main = runInputT defaultSettings $ repl LeftMost []
  where
    repl :: EvalOrder -> Environment -> InputT IO ()
    repl order env = do
      maybeInput <- getInputLine "skic> "
      case Just words <*> maybeInput of
        Nothing -> pure ()
        Just (h : []) | elem h [":?", ":h", ":help"] -> outputStrLn commandList >>= \_ -> repl order env
        Just (q : []) | elem q [":q", ":quit"] -> return ()
        Just (":left" : []) -> outputStrLn ("Now using left-most order of evaluation") >>= \_ -> repl LeftMost env
        Just (":right" : []) -> outputStrLn ("Now using right-most order of evaluation") >>= \_ -> repl RightMost env
        Just (":let" : x : "=" : txt) -> do
          case parser $ alexScanTokens (intercalate " " txt) of
            Left e    -> outputStrLn e >>= \_ -> repl order env
            Right ast -> repl order ((x, ast) : env)
        Just (s : x : [])
          | elem s [":s", ":show"] && lookup x env /= Nothing ->
              tryMaybe (lookup x env) (\ast -> outputStrLn $ show ast) >>= \_ -> repl order env
        Just (e : x : [])
          | elem e [":e", ":eval"] && lookup x env /= Nothing ->
              tryMaybe (lookup x env) (\ast -> let ast' = eval order Total ast in (outputStrLn $ show ast') >>= \_ -> repl order ((x, ast') : env))
        Just (e : n : x : [])
          | elem e [":e", ":eval"] && and (map isDigit n) && lookup x env /= Nothing ->
              tryMaybe (lookup x env) (\ast -> let ast' = eval order (Partial (read n)) ast in outputStrLn (show ast') >>= \_ -> repl order ((x, ast') : env))
        Just (se : []) | elem se [":env", ":showenv"] -> (outputStrLn $ show $ nub $ map fst env) >>= \_ -> repl order env
        Just (equal : x1 : x2 : []) | lookup x1 env /= Nothing && lookup x2 env /= Nothing && elem equal [":eq", ":equal"] -> do
          case (lookup x1 env, lookup x2 env) of
            (Just ast1, Just ast2) | ast1 == ast2 -> outputStrLn (x1 ++ " and " ++ x2 ++ " are sintactically the same")
            _ -> outputStrLn (x1 ++ " and " ++ x2 ++ " are not sintactically the same")
          repl order env
        Just ((':' : _) : _) -> outputStrLn "Command invalid: either variable does not exist, wrong number of arguments or wrong command" >>= \_ -> repl order env
        Just txt -> do
          case parser $ alexScanTokens (intercalate " " txt) of
            Left e -> outputStrLn e
            Right ast -> do
              outputStrLn ("Showing:\n" ++ show ast ++ "\n")
              outputStrLn ("Evaluating:\n" ++ show (eval order Total ast))
          repl order env

tryMaybe :: Maybe a -> (a -> InputT IO ()) -> InputT IO ()
tryMaybe m act = case m of Nothing -> pure (); Just m' -> act m'
