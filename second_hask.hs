--pointInSimplePolygon :: (Num a) => (a,a) -> [((a,a),(a,a))] -> Bool
--pointInSimplePolygon = 
    
data Slope a = Undefined | Infinity | Real a deriving (Show, Eq)

--slopeToMaybeFloat :: Slope Float -> Maybe Float
--slopeToMaybeFloat s = 
--  case s of Undefined -> Nothing
--            Infinity -> Nothing
--            Real a -> Just a
    
lineSegmentToSlope :: (Floating a, Eq a) => (a,a) -> (a,a) -> Slope a
lineSegmentToSlope (x1,y1) (x2,y2) 
  | sameX && sameY = Undefined
  | sameX = Infinity
  | otherwise = Real ((x2 - x1) / (y2 - y1))
  where sameX = x1 == x2
        sameY = y1 == y2
        
slopeToPerpendicular :: Slope Float -> Slope Float
slopeToPerpendicular s = 
    case s of Undefined -> Undefined
              Infinity -> Real 0
              Real 0 -> Infinity
              Real a -> Real (1 / a)
              

    
lineSegmentToLine :: (Floating a, Eq a) => (a,a) -> (a,a) -> ((a,a) -> Bool)
lineSegmentToLine (x1,y1) (x2,y2) = \(x',y') -> s == (s' (x',y'))
  where s = lineSegmentToSlope (x1,y1) (x2,y2)
        s' = lineSegmentToSlope (x1,y1)
