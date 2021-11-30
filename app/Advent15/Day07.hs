module Day07 where

type Wires = Map String (Either Command Word16)

registers :: [String]
registers = (one <$> ['a' .. 'z']) <> join list2 ['a' .. 'z']

vertexOf :: String -> Int
vertexOf x = x `elemIndex` ((one <$> ['a' .. 'z']) <> join list2 ['a' .. 'z']) ?: -1

data Command
  = Constant String
  | And (Either String Word16) String
  | Or (Either String Word16) String
  | Lshift Int String
  | Rshift Int String
  | Complement String
  deriving (Eq, Show)

command :: String -> Wires -> Wires
command w m = case m !? w ?: error "failed to complete: missing key" of
  Left c -> case c of
    Constant w0 -> let m' = command w0 m in alter (const $ m' !? w0) w m'
    And (Left w0) w1 ->
      let m' = command w1 $ command w0 m
       in case mapMaybe (keepRight <=< (m' !?)) [w0, w1] of
            [i, j] -> insert w (Right $ i .&. j) m'
            _ -> error "failed to complete: And Left"
    And (Right i) w1 ->
      let m' = command w1 m
       in case keepRight =<< m' !? w1 of
            Just j -> insert w (Right $ i .&. j) m'
            _ -> error "failed to complete: And Right"
    Or (Left w0) w1 ->
      let m' = command w1 $ command w0 m
       in case mapMaybe (keepRight <=< (m' !?)) [w0, w1] of
            [i, j] -> insert w (Right $ i .|. j) m'
            _ -> error "failed to complete: Or Left"
    Or (Right i) w1 ->
      let m' = command w1 m
       in case keepRight =<< m' !? w1 of
            Just j -> insert w (Right $ i .|. j) m'
            _ -> error "failed to complete: Or Right"
    Lshift s w0 ->
      let m' = command w0 m
       in case keepRight =<< m' !? w0 of
            Just j -> insert w (Right $ shiftL j s) m'
            _ -> error "failed to complete: Lshift"
    Rshift s w0 ->
      let m' = command w0 m
       in case keepRight =<< m' !? w0 of
            Just j -> insert w (Right $ shiftR j s) m'
            _ -> error "failed to complete: Rshift"
    Complement w0 ->
      let m' = command w0 m
       in case keepRight =<< m' !? w0 of
            Just j -> insert w (Right $ complement j) m'
            _ -> error "failed to complete: Complement"
  Right _ -> m

wires :: Parser (String, Either Command Word16)
wires = choice [constant, anded, ored, lshifted, rshifted, complemented]
  where
    constant = do
      d <- register <* string " -> "
      r <- name
      pure (r, (Constant +++ id) d)

    anded = do
      r1 <- register <* string " AND "
      r2 <- name <* string " -> "
      rz <- name
      pure (rz, Left $ And r1 r2)

    ored = do
      r1 <- register <* string " OR "
      r2 <- name <* string " -> "
      rz <- name
      pure (rz, Left $ Or r1 r2)

    lshifted = do
      r1 <- name <* string " LSHIFT "
      n <- number <* string " -> "
      rz <- name
      pure (rz, Left $ Lshift n r1)

    rshifted = do
      r1 <- name <* string " RSHIFT "
      n <- number <* string " -> "
      rz <- name
      pure (rz, Left $ Rshift n r1)

    complemented = do
      r1 <- string "NOT " *> name
      rz <- string " -> " *> name
      pure (rz, Left $ Complement r1)

    lowercase = oneOf ['a' .. 'z']
    name = many1 lowercase
    register = choice [Left <$> name, Right <$> number]

in07 :: Wires
in07 = relist . mapMaybe (parsedWith wires) $ lines (input 2015 7)

part1 :: Word16
part1 = keepRight =<< command "a" in07 !? "a" ?: 0

part2 :: Word16
part2 = keepRight =<< command "a" (insert "b" (Right part1) in07) !? "a" ?: 0
