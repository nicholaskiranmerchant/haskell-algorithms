data Mint = Nan | Neg | IntChar Int
charToMint '0' = IntChar 0
charToMint '1' = IntChar 1
charToMint '2' = IntChar 2
charToMint '3' = IntChar 3
charToMint '4' = IntChar 4
charToMint '5' = IntChar 5
charToMint '6' = IntChar 6
charToMint '7' = IntChar 7
charToMint '8' = IntChar 8
charToMint '9' = IntChar 9
charToMint '-' = Neg
charToMint _ = Nan

stringToMint cs = map charToMint cs
data MintRed = Error | Base | NegMintRed Int | PosMintRed Int Int
reduceMint Nan _ = Error
reduceMint _ Error = Error
reduceMint Neg Base = Error
reduceMint (IntChar n) Base = PosMintRed 1 n
reduceMint _ (NegMintRed _) = Error
reduceMint Neg (PosMintRed _ n) = NegMintRed n
reduceMint (IntChar n) (PosMintRed k m) = PosMintRed k' m'
    where m' = m + ((10 ^ k) * n)
          k' = k + 1

mintsToMintRed ms = foldr reduceMint Base ms

mintRedToInt Error = Nothing
mintRedToInt Base = Nothing
mintRedToInt (NegMintRed n) = Just (-1 * n)
mintRedToInt (PosMintRed _ n) = Just n

stringToInt = mintRedToInt . mintsToMintRed . stringToMint


