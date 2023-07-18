import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

resolveMap :: Maybe.Maybe Integer -> Integer
resolveMap n = ((Maybe.maybe 0) id) n

addFibbToMap :: Int -> (Map.Map Int Integer -> Map.Map Int Integer)
addFibbToMap n m = do
    let f1 = (Map.lookup (n - 1)) m
    let f2 = (Map.lookup (n - 2)) m 
    if (Maybe.isJust f1) && (Maybe.isJust f2)  
      then ((Map.insert n) (resolveMap f1 + resolveMap f2)) m
      else m
      
fibbMap :: Int -> Map.Map Int Integer
fibbMap n = ((foldr addFibbToMap) (Map.fromList [(0,0),(1,1)])) [n,n-1..2]

fastFibb :: Int -> Integer
fastFibb n = resolveMap ((Map.lookup n) (fibbMap n))

slowFibb :: Int -> Integer
slowFibb 0 = 0
slowFibb 1 = 1
slowFibb n = (slowFibb (n - 1)) + (slowFibb (n - 2))

slowEqualsFast :: [Bool]
slowEqualsFast = (map (\n -> (fastFibb n) == (slowFibb n))) [1..]
