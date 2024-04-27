
--UPDATE LIST: https://www.hackerrank.com/challenges/fp-update-list/problem

module Main where

    -- Enter your code here. Read input from STDIN. Print output to STDOUT
    f :: [Int] -> [Int]
    f (x:xs) = if x < 0 then x*(-1):f xs else x:f xs

    -- This section handles the Input/Output and can be used as it is. Do not modify it.
    main = do
        inputdata <- getContents
        mapM_ putStrLn $ map show $ f $ map (read :: String -> Int) $ lines inputdata