{-# LANGUAGE TemplateHaskell #-}

module Day23 where

import Advent
import Control.Lens (makeLenses)
import Data.Vector qualified as V

type Register = Char

type Memory = Map Register Natural

data Computer = Computer
  { _cursor :: Int,
    _memory :: Memory
  }
  deriving (Eq, Show)

makeLenses ''Computer

data Instruction
  = Hlf Register
  | Tpl Register
  | Inc Register
  | Jmp Int
  | Jie Register Int
  | Jio Register Int
  deriving (Eq, Show)

perform :: Instruction -> Computer -> Computer
perform = \case
  Hlf r -> (memory %~ alter (fmap (`div` 2)) r) . (cursor %~ succ)
  Tpl r -> (memory %~ alter (fmap (* 3)) r) . (cursor %~ succ)
  Inc r -> (memory %~ alter (fmap succ) r) . (cursor %~ succ)
  Jmp n -> cursor %~ (+ n)
  Jie r n -> bool (cursor %~ succ) (cursor %~ (+ n)) =<< (maybe False even . lookup r . view memory)
  Jio r n -> bool (cursor %~ succ) (cursor %~ (+ n)) =<< ((Just 1 ==) . lookup r . view memory)

instruction :: Parser Instruction
instruction = choice [hlf, tpl, inc, jmp, jie, jio]
  where
    hlf = fmap Hlf $ string "hlf " *> oneOf "ab"
    tpl = fmap Tpl $ string "tpl " *> oneOf "ab"
    inc = fmap Inc $ string "inc " *> oneOf "ab"
    jmp = fmap Jmp $ string "jmp " *> choice [id <$ char '+', negate <$ char '-'] <*> number
    jie = do
      r <- string "jie " *> oneOf "ab" <* string ", "
      f <- choice [id <$ char '+', negate <$ char '-']
      Jie r . f <$> number
    jio = do
      r <- string "jio " *> oneOf "ab" <* string ", "
      f <- choice [id <$ char '+', negate <$ char '-']
      Jio r . f <$> number

in23 :: Vector Instruction
in23 = relist . mapMaybe (parsedWith instruction) $ lines (input 2015 23)

compute :: Vector Instruction -> Computer -> Computer
compute program c = case program V.!? _cursor c of
  Nothing -> c
  Just instr -> compute program $! perform instr c

part1 :: Natural
part1 = (compute in23 (Computer 0 $ relist [(c, 0) | c <- "ab"]) ^. memory) !? 'b' ?: 0

part2 :: Natural
part2 = (compute in23 (Computer 0 $ relist [(c, (c == 'a') ^. enum) | c <- "ab"]) ^. memory) !? 'b' ?: 0
