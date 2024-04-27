-- CONVEX HULL : https://www.hackerrank.com/challenges/convex-hull-fp/problem 

module ConvexHull where

    type Point = (Int,Int)

    dotProduct :: Point -> Point -> Point -> Int
    dotProduct (ax,ay) (bx,by) (cx, cy) = (bx-ax)*(cy-ay)-(by-ay)*(cx-ax)

    side :: Point -> Point -> Point -> Bool -> Bool
    side a b c True = dotProduct a b c > 0
    side a b c False = dotProduct a b c < 0

    f :: [Point] -> Double
    f pts = long (hull [] pts False) + long (hull [] pts True)

    hull :: [Point] -> [Point] -> Bool -> [Point]
    hull xs [] _ = xs
    hull [] (x:xs) left = hull [x] xs left
    hull [d] (x:xs) left = hull (x:[d]) xs left
    hull (d1:d2:ds) (x:xs) left
        | side d1 d2 x left = hull (x:d1:d2:ds) xs left
        | otherwise = hull (d2:ds) (x:xs) left

    long :: [Point] -> Double
    long [x] = 0
    long (x:y:xs) = long (y:xs) + dotDistance x y

    dotDistance :: Point -> Point -> Double
    dotDistance (xa,ya) (xb,yb) = sqrt ( fromIntegral ((xa-xb)^2 + (ya-yb)^2))

    examplePoints :: [Point]
    examplePoints = [(1, 1), (2, 5), (3, 3), (5, 3), (3, 2)]