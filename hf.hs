#!/usr/bin/env stack
-- stack --resolver lts-13.7 script

import Data.Map.Strict as Map

func :: (Int a) => Map a a -> Map a a
func a = a



main :: IO()
main = do
    line <- getLine
    print line
