-- https://www.hackerrank.com/challenges/lambda-march-concave-polygon/problem

module ConcavePolygon where

    import           Data.List (minimumBy, sortBy)
    import           Data.Ord  (comparing)

    data Direction = Clockwise
                | CounterClockwise
                | Collinear
                deriving (Eq, Show)

    type Coordinate = (Double, Double)
    type TriplePoint = (Coordinate, Coordinate, Coordinate)

    direction :: TriplePoint -> Direction
    direction ((x1, y1), (x2, y2), (x3, y3)) =
        case compare calc 0 of
        GT -> CounterClockwise
        LT -> Clockwise
        EQ -> Collinear
        where calc = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

    directionOrd :: Direction -> Ordering
    directionOrd CounterClockwise = GT
    directionOrd Clockwise = LT
    directionOrd Collinear = EQ

    polarSort :: [Coordinate] -> [Coordinate]
    polarSort xs =
        pivot:(sortBy (\x y -> directionOrd $ direction (y, pivot, x)) . filter (pivot /=)) xs
        where
        pivot = lowestYPoint xs

    lowestYPoint :: [Coordinate] -> Coordinate
    lowestYPoint = minimumBy (comparing yCoord)
        where yCoord (_, y) = y

    segments :: [Coordinate] -> [TriplePoint]
    segments [] = []
    segments as@(x:y:_) = zip3 as fromSecond fromThird
        where fromSecond = tail $ as ++ [x]
                    fromThird = tail $ fromSecond ++ [y]

    concave :: [Direction] -> Bool
    concave xs = not (allClockwise relevantDirections || allCounterClockwise relevantDirections)
    where relevantDirections = filter (/= Collinear) xs
            allClockwise = all (== Clockwise)
            allCounterClockwise = all (== CounterClockwise)

    inputData :: String -> [Coordinate]
    inputData = toTuple . map words . tail . lines
    where toTuple xs = [(read x, read y) | (x:y:_) <- xs]

    main :: IO ()
    main = do
        input <- getContents
        (printYesNo . concave . map direction) (segments $ polarSort (inputData input))
    where
        printYesNo True = putStrLn "YES"
        printYesNo False = putStrLn "NO"

