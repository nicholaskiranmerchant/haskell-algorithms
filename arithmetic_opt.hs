

data Arth = Operator ([Integer] -> Arth) [Arth] | Operand Integer

resolve :: Arth -> Integer
resolve (Operator f xs) = (f . (map resolve)) xs
resolve (Operand n) = n



expression = (Operator sum [])