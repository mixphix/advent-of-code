module Day25 where

import Advent
import Data.Set qualified as Set

type Tape = Set Integer

type Turing = (Integer, Tape, Char)

turing :: Char -> Turing
turing c = (0, Empty, c)

data Move = Lf | Rg deriving (Eq, Show)

data Write = Z | O deriving (Eq, Show)

data Instruction = Instruction Bool Move Write Char deriving (Eq, Show)

stateP :: Parser (Map Char [Instruction])
stateP = do
  s0 <- string "In state " *> anyChar <* string ":" <* many space
  on0 <- many space *> string "If the current value is 0:" *> many space *> substate
  on1 <- many space *> string "If the current value is 1:" *> many space *> substate
  pure $ one (s0, [uncurry3 (Instruction False) on0, uncurry3 (Instruction True) on1])
  where
    substate = do
      write <- many space *> string "- Write the value " *> number <* string "."
      move <- many space *> string "- Move one slot to the " *> many letter <* string "."
      s' <- many space *> string "- Continue with state " *> anyChar <* string "."
      pure
        ( case move of "left" -> Lf; _ -> Rg,
          case write :: Integer of 1 -> O; _ -> Z,
          s'
        )

turingP :: Parser (Char, Int, Map Char [Instruction])
turingP = do
  st8 <- string "Begin in state " *> anyChar <* string "." <* many space
  steps <- string "Perform a diagnostic checksum after " *> number <* string " steps." <* many space
  transitions <- stateP `sepEndBy` many space
  pure (st8, steps, fold transitions)

transition :: Map Char [Instruction] -> Turing -> Turing
transition ts (p, t, s) = case ts !? s ?: [] of
  [] -> (p, t, s)
  is -> case [(m, w, c) | Instruction b m w c <- is, b == Set.member p t] of
    [(m, w, c)] -> ((case m of Lf -> pred; _ -> succ) p, case w of O -> Set.insert p t; _ -> Set.delete p t, c)
    _ -> (p, t, s)

run :: (Char, Int, Map Char [Instruction]) -> Turing
run (c, steps, ts) = applyN steps (transition ts) (turing c)

in25 :: Either ParseError (Char, Int, Map Char [Instruction])
in25 = parse turingP "" (input 2017 25)

part1 :: Natural
part1 = either (const 0) (count every . view _2 . run) in25
