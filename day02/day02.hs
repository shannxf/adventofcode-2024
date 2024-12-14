-- https://adventofcode.com/2024/day/2
module Day02 where

import Data.List (delete)

-- input from a txt file
inputFromTxt :: FilePath -> IO [[Int]]
inputFromTxt filePath = do
    contents <- readFile filePath
    let fileLines = lines contents
    return $ map (map read . words) fileLines

-- check if is a increasing safe report
isIncReport :: [Int] -> Bool
isIncReport [] = True
isIncReport [_] = True
isIncReport (x:y:rest) = isIncReport (y:rest) && y - x >= 1 && y - x <= 3

-- check if is a decreasing safe report
isDecReport :: [Int] -> Bool
isDecReport [] = True
isDecReport [_] = True
isDecReport (x:y:rest) = isDecReport (y:rest) && x - y >= 1 && x - y <= 3

-- solution of part1
solutionQ1 :: IO ()
solutionQ1 = do
    reports <- inputFromTxt "input.txt"
    print $ length (filter (\report -> isIncReport report || isDecReport report) reports)

-- delete an element at a specific index from a list
removeAtIndex :: Int -> [a] -> [a]
removeAtIndex n xs = take n xs ++ drop (n + 1) xs

-- Generic function to check a report with one level removed
isReportWithTolerance :: ([Int] -> Bool) -> [Int] -> Bool
isReportWithTolerance baseCheck report = 
    baseCheck report || any baseCheck [removeAtIndex i report | i <- [0 .. length report - 1]]

-- Check if a report is safe with tolerance
isSafeReportWithTolerance :: [Int] -> Bool
isSafeReportWithTolerance report =
    isReportWithTolerance isIncReport report || isReportWithTolerance isDecReport report

-- Main solution function
solutionQ2 :: IO ()
solutionQ2 = do
    reports <- inputFromTxt "input.txt"
    print $ length (filter isSafeReportWithTolerance reports)
