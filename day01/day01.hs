-- https://adventofcode.com/2024/day/1
module Day01 where

import Data.List (sort)

distanceOfTwoLists :: [Int] -> [Int] -> Int
distanceOfTwoLists ls1 ls2 = sum (zipWith (\ x1 x2 -> abs (x1 - x2)) (sort ls1) (sort ls2))

parseLines :: [String] -> ([Int], [Int])
parseLines = foldr parseLine ([], [])
    where
        parseLine :: String -> ([Int], [Int]) -> ([Int], [Int])
        parseLine line (list1, list2) =
            let [number1, number2] = map read (words line)
            in (number1 : list1, number2 : list2)

inputFromTxt :: FilePath -> IO ([Int], [Int])
inputFromTxt filePath = do
    contents <- readFile filePath
    let linesOfFile = lines contents
    return (parseLines linesOfFile)

solutionQ1 :: IO ()
solutionQ1 = do
    (list1, list2) <- inputFromTxt "input.txt"
    let result = distanceOfTwoLists list1 list2
    print result

similarityScore :: [Int] -> [Int] -> Int
similarityScore list1 list2 = foldr calculateForEachNum 0 list1
    where
        calculateForEachNum :: Int -> Int -> Int
        calculateForEachNum x result = result + x * length (filter (== x) list2)

solutionQ2 :: IO ()
solutionQ2 = do
    (list1, list2) <- inputFromTxt "input.txt"
    let result = similarityScore list1 list2
    print result
