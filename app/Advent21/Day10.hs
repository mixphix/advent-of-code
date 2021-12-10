module Day10 where

in10 :: [String]
in10 = toString <$> lines (input 2021 10)

opens :: String
opens = "([{<"

illegalCharacter :: String -> Maybe Char
illegalCharacter = go []
  where
    go _ [] = Nothing
    go [] (c : cs)
      | c `elem` opens = go [c] cs
      | otherwise = Just c
    go (s : ss) (c : cs) = case (s, c) of
      ('(', ')') -> go ss cs
      ('[', ']') -> go ss cs
      ('{', '}') -> go ss cs
      ('<', '>') -> go ss cs
      _ -> if c `elem` opens then go (c : s : ss) cs else Just c

syntaxErrorScore :: Char -> Natural
syntaxErrorScore ')' = 3
syntaxErrorScore ']' = 57
syntaxErrorScore '}' = 1197
syntaxErrorScore '>' = 25137
syntaxErrorScore x = error $ "unexpected character: " <> one x

part1 :: Natural
part1 = sumOn (maybe 0 syntaxErrorScore . illegalCharacter) in10

complete :: String -> String
complete = go []
  where
    go xs [] = map bracket xs
    go [] (c : cs)
      | c `elem` opens = go [c] cs
      | otherwise = []
    go (s : ss) (c : cs) = case (s, c) of
      ('(', ')') -> go ss cs
      ('[', ']') -> go ss cs
      ('{', '}') -> go ss cs
      ('<', '>') -> go ss cs
      _ -> if c `elem` opens then go (c : s : ss) cs else []

    bracket :: Char -> Char
    bracket = \case
      '(' -> ')'
      '[' -> ']'
      '{' -> '}'
      '<' -> '>'
      _ -> error "not a bracket"

autocompleteScore :: String -> Natural
autocompleteScore = go 0
  where
    go n [] = n
    go n (c : cs) = (`go` cs) . ((5 * n) +) $ case c of
      ')' -> 1
      ']' -> 2
      '}' -> 3
      '>' -> 4
      x -> error $ "unexpected character: " <> one x

part2 :: Natural
part2 =
  let scores = sort . map (autocompleteScore . complete) $ filter (isNothing . illegalCharacter) in10
   in withNonEmpty 0 head $ drop ((length scores - 1) `div` 2) scores
