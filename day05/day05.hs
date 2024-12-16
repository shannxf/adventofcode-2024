module Day05 where

import Data.Char (isDigit)
import Data.List (elemIndex)

-- read the file and get the lines
inputFromTxt :: FilePath -> IO [String]
inputFromTxt filePath = do
    contents <- readFile filePath
    return $ lines contents

-- get the tuple of: ([rule], [update])
parseInput :: [String] -> ([(Int, Int)], [[Int]])
parseInput = foldr parseLine ([], [])
    where
        parseLine :: String -> ([(Int, Int)], [[Int]]) -> ([(Int, Int)], [[Int]])
        parseLine line (rules, updates) =
            case span isDigit line of
                (x, '|':y)      -> ((read x, read y) : rules, updates)
                (x, ',':rest)   -> (rules, parseUpdate line : updates)
                _               -> (rules, updates)

-- parse a line of update like "25,62,49,15,28,65,35"
parseUpdate :: String -> [Int]
parseUpdate str =
    case span isDigit str of
        (x, ',':rest)   -> read x : parseUpdate rest
        (x, [])         -> [read x]


-- check if a legal is legal according to the sorted rules
isLegalUpdate :: [Int] -> [(Int, Int)] -> Bool
isLegalUpdate []            _       = True
isLegalUpdate [_]           _       = True
isLegalUpdate (x:y:rest)    rules   =
    elem (x, y) rules && isLegalUpdate (y:rest) rules


-- get the middle page number of a legal update
getUpdateMiddle :: [Int] -> Int
getUpdateMiddle update = update !! (length update `div` 2)

solutionQ1 :: IO ()
solutionQ1 = do
    fileLines <- inputFromTxt "input.txt"
    let
        (rules, updates) = parseInput fileLines
        result = foldr p 0 updates

        p :: [Int] -> Int -> Int
        p update current_result
            | isLegalUpdate update rules = current_result + getUpdateMiddle update
            | otherwise = current_result
    print result

-- recursively fix the illegal update
transferToLegalUpdate :: [Int] -> [(Int, Int)] -> [Int]
transferToLegalUpdate update rules
    | isLegalUpdate update rules = update
    | otherwise = transferToLegalUpdate (oneStepTransfer update) rules
        where
            oneStepTransfer :: [Int] -> [Int]
            oneStepTransfer [x]         = [x]
            oneStepTransfer (x:y:rest)
                | (x, y) `elem` rules   = x : oneStepTransfer (y:rest)
                | otherwise             = y : x : rest

solutionQ2 :: IO ()
solutionQ2 = do
    fileLines <- inputFromTxt "input.txt"
    let
        (rules, updates) = parseInput fileLines
        result = foldr p 0 updates

        p :: [Int] -> Int -> Int
        p update current_result
            | not $ isLegalUpdate update rules = current_result + getUpdateMiddle (transferToLegalUpdate update rules)
            | otherwise = current_result

    print result