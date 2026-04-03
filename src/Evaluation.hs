module Evaluation where

import           Syntax

data EvalOrder
  = LeftMost
  | RightMost
  deriving (Eq)

data EvalNumber
  = Total
  | Partial Int
  deriving (Eq)

eval1 :: EvalOrder -> TermNode -> Maybe TermNode
eval1 order (TermNode fi tm) =
  Just (TermNode fi)
    <*> case tm of
      TmApp (TermNode _ TmIComb) t2 -> Just (getTm t2)
      TmApp (TermNode _ (TmApp (TermNode _ TmKComb) t12)) _ -> Just (getTm t12)
      TmApp (TermNode _ (TmApp (TermNode _ (TmApp (TermNode _ TmSComb) t112)) t12)) t2 -> Just (TmApp (TermNode fi (TmApp t112 t2)) (TermNode fi (TmApp t12 t2)))
      TmApp t1 t2 ->
        case order of
          LeftMost ->
            case eval1 order t1 of
              Nothing  -> Just (\t2' -> TmApp t1 t2') <*> eval1 order t2
              Just t1' -> Just (TmApp t1' t2)
          RightMost ->
            case eval1 order t2 of
              Nothing  -> Just (\t1' -> TmApp t1' t2) <*> eval1 order t1
              Just t2' -> Just (TmApp t1 t2')
      _ -> Nothing

eval :: EvalOrder -> EvalNumber -> TermNode -> TermNode
eval order Total t =
  case eval1 order t of
    Nothing -> t
    Just t' -> eval order Total t'
eval order (Partial n) t
  | n <= 0 = t
  | otherwise =
      case eval1 order t of
        Nothing -> t
        Just t' -> eval order (Partial (n - 1)) t'
