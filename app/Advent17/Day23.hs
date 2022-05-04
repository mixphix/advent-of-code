{-# LANGUAGE TemplateHaskell #-}

module Day23 where

import Control.Lens (makeLenses, (+~))
import GHC.Show qualified (show)

data Instruction
  = Set Text (Either Text Integer)
  | Sub Text (Either Text Integer)
  | Mul Text (Either Text Integer)
  | Jnz (Either Text Integer) (Either Text Integer)
  deriving (Eq, Show)

instructionP :: Parser Instruction
instructionP = choice [setP, subP, mulP, jnzP]
 where
  eti :: Parser (Either Text Integer)
  eti = choice [Right <$> number, Left . toText <$> many1 letter]

  setP = do
    void $ string "set "
    r <- toText <$> many1 letter
    Set r <$> (string " " *> eti)
  subP = do
    void $ string "sub "
    r <- toText <$> many1 letter
    Sub r <$> (string " " *> eti)
  mulP = do
    void $ string "mul "
    r <- toText <$> many1 letter
    Mul r <$> (string " " *> eti)
  jnzP = do
    void $ string "jnz "
    r <- eti
    Jnz r <$> (string " " *> eti)

in23 :: [Instruction]
in23 = parse instructionP <$> lines (input 2017 23)

data Computer = Computer
  { _registers :: Map Text Integer
  , _instrpos :: Int
  }
  deriving (Eq)

makeLenses ''Computer

instance Show Computer where
  show Computer{..} =
    "Computer (" <> show _instrpos <> ") "
      <> foldMap (\(t, i) -> "{" <> toString t <> ": " <> show i <> "}") (relist @_ @[] _registers)

instruction :: (Integer, Computer) -> [Instruction] -> Maybe (Integer, Computer)
instruction (s, c@Computer{..}) is = case is !!? _instrpos of
  Nothing -> Nothing
  Just i -> Just $ case i of
    Set x e -> (s, c' & registers %~ (ix x .~ either reg id e))
    Sub x e -> (s, c' & registers %~ (at x %~ Just . subtract (either reg id e) . (?: 0)))
    Mul x e -> (succ s, c' & registers %~ (at x %~ Just . (* either reg id e) . (?: 0)))
    Jnz x e
      | either reg id x /= 0 -> (s, c & instrpos +~ fromIntegral (either reg id e))
      | otherwise -> (s, c')
   where
    c' = c & instrpos %~ succ
    reg :: Text -> Integer
    reg z = _registers !? z ?: 0

part1 :: Integer
part1 = withNonEmpty 0 last $ unfoldr (\(n, c) -> (n,) <$> instruction (n, c) in23) (0, Computer m 0)
 where
  m = relist [(c, 0) | c <- map one "abcdefgh"]

part2 :: Natural
part2 = count (not . prime) $ take 1001 [fromIntegral b, fromIntegral b + 17 ..]
 where
  m = relist [(c, fromIntegral $ fromEnum (c == "a")) | c <- map one "abcdefgh"]
  Just b =
    (((!? "b") . _registers) <=< viaNonEmpty head) . dropWhile ((< Just 100000) . (!? "b") . _registers) $
      unfoldr (\(n, c) -> (c,) <$> instruction (n, c) in23) (0, Computer m 0)
