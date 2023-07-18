import Data.List

data Point = Point Double Double
data Vector = Vector Double Double
data Direction = L | S | R

pointsToVector (Point x y) (Point x' y') = Vector (x' - x) (y' - y)
vectorCrossProduct (Vector i j) (Vector i' j') = (i * j') - (j * i')

pointsCrossProduct p0 p1 p2 = vectorCrossProduct u v 
    where u = pointsToVector p0 p1
          v = pointsToVector p0 p2

crossProductToDirection a
    | a >  0 = L  
    | a == 0 = S
    | a <  0 = R

leansToThe p0 p1 p2 = crossProductToDirection (pointsCrossProduct p0 p1 p2) 

lowestPoint xs = head (sortOn yVal xs)
    where yVal = \(Point _ y) -> y

dotProduct (Vector i j) (Vector i' j') = (i * i') + (j * j')
absoluteValue (Vector i j) = sqrt ((i ^ 2) + (j ^ 2)) 

cosine v v' = dot / abs
    where dot = dotProduct v v'
          abs = (absoluteValue v) * (absoluteValue v')

polarOrder ps = sortOn polar ps 
    where lowest = lowestPoint ps
          base   = (Vector 1 0)
          polar  = \p -> cosine base (pointsToVector lowest p)

data Hull p = Hull p p [p] | Single p | Empty

tailHull Empty = Empty
tailHull (Single p) = Empty
tailHull (Hull p' p []) = Single p
tailHull (Hull p2 p1 (p0:ps)) = Hull p1 p0 ps

reduceHull p Empty = Single p
reduceHull p' (Single p) = Hull p' p []
reduceHull p2 (Hull p1 p0 ps) = 
    case lean of
        L -> Hull p2 p1 (p0:ps)
        _ -> reduceHull p2 tail 
    where lean = leansToThe p0 p1 p2 
          tail = tailHull (Hull p1 p0 ps)

grahamScan ps = foldr reduceHull Empty ps

tuplesToPoints ts = map (\(x,y) -> Point x y) ts
pointsToTuples ps =  map (\(Point x y) -> (x, y)) ps

printHull (Hull p1 p0 ps) = Just (pointsToTuples (p1:p0:ps))
printHull _ = Nothing

points1 = tuplesToPoints [(0,0),(1,1),(2,3),(7,2),(-1,6),(2,0.5),(1.2,0.2),(-5,3)]
points2 = tuplesToPoints [(0,-1),(1,0),(0,1),(-1,0),(0.4,0.4)]
run points = do
    print (pointsToTuples points)
    print (pointsToTuples (polarOrder points))
    print (printHull (grahamScan (polarOrder points)))

main = do (run points1) 