import Data.Char  
import Data.List 

main = do line <- fmap (map toUpper) getLine
          putStrLn line
