module Day10 where

data Bot
  = Bot Natural
  | Output Natural
  deriving (Eq, Ord, Show)

data Command
  = Value Bot Natural
  | Give Bot Bot Bot
  deriving (Eq, Ord, Show)

command :: Parser Command
command = choice [value, give]
  where
    value = do
      val <- string "value " *> number
      b <- string " goes to " *> bot
      pure $ Value b val

    give = do
      has <- bot
      lo <- string " gives low to " *> choice [bot, output]
      hi <- string " and high to " *> choice [bot, output]
      pure $ Give has lo hi

    bot = Bot <$> (string "bot " *> number)
    output = Output <$> (string "output " *> number)

type Factory = Map Bot [Natural]

execute :: Part -> [Command] -> Factory -> Either Bot Factory
execute part (Value b n : cs) m = execute part cs $ insertWith (<>) b [n] m
execute part (g@(Give has lo hi) : cs) m
  | Just (sort -> [l, h]) <- m !? has =
    if [l, h] == [17, 61] && part == Part1
      then Left has
      else
        execute part cs
          . insertWith (<>) lo [l]
          . insertWith (<>) hi [h]
          $ delete has m
  | otherwise = execute part (cs <> [g]) m
execute _ _ m = Right m

in10 :: [Command]
in10 = sort . mapMaybe (parsedWith command) $ lines (input 2016 10)

part1 :: Natural
part1 =
  let Left (Bot bot) = execute Part1 in10 Empty
   in bot

part2 :: Natural
part2 =
  let Right m = execute Part2 in10 Empty
   in product $ foldMap (?: []) [m !? Output 0, m !? Output 1, m !? Output 2]
