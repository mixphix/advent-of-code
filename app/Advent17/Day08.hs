module Day08 where

data Comparing
  = LessThan Int
  | GreaterThan Int
  | LessThanOrEqual Int
  | GreaterThanOrEqual Int
  | Equal Int
  | NotEqual Int
  deriving (Eq, Show)

testComparing :: Comparing -> Text -> Map Text Int -> Bool
testComparing cmp k m =
  let val = m !? k ?: 0
   in case cmp of
        LessThan v -> val < v
        GreaterThan v -> val > v
        LessThanOrEqual v -> val <= v
        GreaterThanOrEqual v -> val >= v
        Equal v -> val == v
        NotEqual v -> val /= v

data Instruction
  = Increment Text Int Text Comparing
  | Decrement Text Int Text Comparing

comparingP :: Parser Comparing
comparingP = choice [lessthan, greaterthan, lessthanorequal, greaterthanorequal, equal, notequal]
  where
    lessthan = LessThan <$> (string " < " *> number)
    lessthanorequal = LessThanOrEqual <$> (string " <= " *> number)
    greaterthan = GreaterThan <$> (string " > " *> number)
    greaterthanorequal = GreaterThanOrEqual <$> (string " >= " *> number)
    equal = Equal <$> (string " == " *> number)
    notequal = NotEqual <$> (string " != " *> number)

instructionP :: Parser Instruction
instructionP = do
  reg1 <- toText <$> many letter
  instr <- choice [Increment <$ string " inc ", Decrement <$ string " dec "]
  qty <- number
  reg2 <- string " if " *> (toText <$> many letter)
  instr reg1 qty reg2 <$> comparingP

in08 :: [Instruction]
in08 = parse instructionP <$> lines (input 2017 8)

execute :: Instruction -> Map Text Int -> Map Text Int
execute i m = case i of
  Increment reg1 qty reg2 cmp ->
    if testComparing cmp reg2 m
      then alter (Just . maybe qty (+ qty)) reg1 m
      else m
  Decrement reg1 qty reg2 cmp ->
    if testComparing cmp reg2 m
      then alter (Just . maybe (-qty) (subtract qty)) reg1 m
      else m

part1 :: Int
part1 = withNonEmpty 0 maximum1 $ foldl' (flip execute) Empty in08

part2 :: Int
part2 = withNonEmpty 0 (maximumOf1 (withNonEmpty 0 maximum1)) $ scanl' (flip execute) Empty in08
