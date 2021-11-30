{-# LANGUAGE TemplateHaskell #-}

module Day18 where

import Control.Lens (makeLenses, (+~))
import Control.Monad.Toolbox (loop)
import GHC.Show qualified

data Instruction
  = Snd (Either Text Integer)
  | Set Text (Either Text Integer)
  | Add Text (Either Text Integer)
  | Mul Text (Either Text Integer)
  | Mod Text (Either Text Integer)
  | Rcv Text
  | Jgz (Either Text Integer) (Either Text Integer)
  deriving (Eq, Show)

instructionP :: Parser Instruction
instructionP = choice [soundP, setP, addP, mulP, modP, recoverP, jgzP]
  where
    eti :: Parser (Either Text Integer)
    eti = choice [Right <$> number, Left . toText <$> many1 letter]

    soundP = Snd <$> (string "snd " *> eti)
    setP = do
      void $ string "set "
      r <- toText <$> many1 letter
      Set r <$> (string " " *> eti)
    addP = do
      void $ string "add "
      r <- toText <$> many1 letter
      Add r <$> (string " " *> eti)
    mulP = do
      void $ string "mul "
      r <- toText <$> many1 letter
      Mul r <$> (string " " *> eti)
    modP = do
      void $ string "mod "
      r <- toText <$> many1 letter
      Mod r <$> (string " " *> eti)
    recoverP = do
      void $ string "rcv "
      Rcv . toText <$> many1 letter
    jgzP = do
      void $ string "jgz "
      r <- eti
      Jgz r <$> (string " " *> eti)

data Computer = Computer
  { _registers :: Map Text Integer,
    _sound :: [Integer],
    _instrpos :: Int
  }
  deriving (Eq)

makeLenses ''Computer

instance Show Computer where
  show Computer {..} =
    "Computer (" <> show _instrpos <> ") "
      <> foldMap (\(t, i) -> "{" <> toString t <> ": " <> show i <> "}") (relist @_ @[] _registers)
      <> " "
      <> show _sound

queue :: ([Integer] -> Identity [Integer]) -> Computer -> Identity Computer
queue = sound

instruction :: Part -> (Maybe Integer, Computer) -> [Instruction] -> Maybe (Maybe Integer, Computer)
instruction Part1 (s, c@Computer {..}) is = Just $ case is !!? _instrpos of
  Nothing -> (s, c)
  Just i -> case i of
    Snd x -> (s, c' & sound %~ (either reg id x :))
    Set x e -> (s, c' & registers %~ (ix x .~ either reg id e))
    Add x e -> (s, c' & registers %~ (at x %~ Just . (either reg id e +) . (?: 0)))
    Mul x e -> (s, c' & registers %~ (at x %~ Just . (either reg id e *) . (?: 0)))
    Mod x e -> (s, c' & registers %~ (at x %~ Just . (`mod` either reg id e) . (?: 0)))
    Rcv x
      | reg x /= 0 -> (viaNonEmpty head _sound, c')
      | otherwise -> (s, c')
    Jgz x e
      | either reg id x > 0 -> (s, c & instrpos +~ fromIntegral (either reg id e))
      | otherwise -> (s, c')
    where
      c' = c & instrpos %~ succ
      reg :: Text -> Integer
      reg z = _registers !? z ?: 0
instruction Part2 (s, c@Computer {..}) is = case is !!? _instrpos of
  Nothing -> Nothing
  Just i -> case i of
    Snd x -> Just (Just $ either reg id x, c')
    Set x e -> Just (s, c' & registers %~ (ix x .~ either reg id e))
    Add x e -> Just (s, c' & registers %~ (at x %~ Just . (either reg id e +) . (?: 0)))
    Mul x e -> Just (s, c' & registers %~ (at x %~ Just . (either reg id e *) . (?: 0)))
    Mod x e -> Just (s, c' & registers %~ (at x %~ Just . (`mod` either reg id e) . (?: 0)))
    Rcv x -> case _sound of
      [] -> Nothing
      (q : qs) -> Just (s, c' & registers %~ (ix x .~ q) & queue .~ qs)
    Jgz x e
      | either reg id x > 0 -> Just (s, c & instrpos +~ fromIntegral (either reg id e))
      | otherwise -> Just (s, c')
    where
      c' = c & instrpos %~ succ
      reg :: Text -> Integer
      reg z = _registers !? z ?: 0

in18 :: [Instruction]
in18 = mapMaybe (parsedWith instructionP) $ lines (input 2017 18)

part1 :: Integer
part1 =
  fromMaybe 0 . getAlt . foldMap (Alt . fst) $
    iterate' (\c -> instruction Part1 c in18 ?: error "boo") (empty, Computer m empty 0)
  where
    m = relist [(c, 0) | c <- map one "abfip"]

data DuetState
  = Blocked0
  | Blocked1
  | Working
  deriving (Show)

duet :: (Int, DuetState, Computer, Computer)
duet = (0, Working, Computer m empty 0, Computer (m & ix "p" .~ 1) empty 0)
  where
    m = relist [(c, 0) | c <- map one "abfip"]

synchronous :: (Int, DuetState, Computer, Computer) -> [Instruction] -> Either (Int, DuetState, Computer, Computer) Int
synchronous (n, ds, c0, c1) is = case ds of
  Blocked0 -> case instruction Part2 (Nothing, c1) is of
    Nothing -> Right n
    Just (Nothing, c1') -> Left (n, Blocked0, c0, c1')
    Just (Just sent, c1') -> synchronous (succ n, Working, c0 & queue <>~ [sent], c1') is
  Blocked1 -> case instruction Part2 (Nothing, c0) is of
    Nothing -> Right n
    Just (Nothing, c0') -> Left (n, Blocked1, c0', c1)
    Just (Just sent, c0') -> Left (n, Working, c0', c1 & queue <>~ [sent])
  Working -> case instruction Part2 (Nothing, c0) is of
    Nothing -> Left (n, Blocked0, c0, c1)
    Just (Nothing, c0') -> Left (n, Working, c0', c1)
    Just (Just sent, c0') -> Left (n, Working, c0', c1 & queue <>~ [sent])

part2 :: Int
part2 = loop (`synchronous` in18) duet
