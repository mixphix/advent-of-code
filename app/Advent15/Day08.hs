module Day08 where

stringcode :: Parser String
stringcode = many1 anyChar <&> drop 1 . dropEnd 1

stringmem :: String -> Int
stringmem ('\\' : 'x' : _h1 : _h2 : rest) = let y = stringmem rest in 1 + y
stringmem ('\\' : '\"' : rest) = let y = stringmem rest in 1 + y
stringmem ('\\' : '\\' : rest) = let y = stringmem rest in 1 + y
stringmem (_ : rest) = let y = stringmem rest in 1 + y
stringmem [] = 0

in08 :: [(String, Int, Int, Int)]
in08 =
  lines (input 2015 8)
    <&> ( \x ->
            ( x
            , 2 + length x
            , stringmem x
            , length . show @String $ "\"" <> x <> "\""
            )
        )
      . parse stringcode

part1 :: Int
part1 = liftM2 (-) (sumOn $ view _2) (sumOn $ view _3) in08

part2 :: Int
part2 = liftM2 (-) (sumOn $ view _4) (sumOn $ view _2) in08
