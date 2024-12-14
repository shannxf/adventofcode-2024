module Day04 where

import Data.List (transpose, isPrefixOf)

-- input by lines
inputHorizontalFromTxt :: FilePath -> IO [String]
inputHorizontalFromTxt filePath = do
    contents <- readFile filePath
    return $ lines contents

-- count all horizontal "XMAS" in the contents
countHorizontal :: [String] -> Int
countHorizontal = sum . map countLine
    where
        countLine [] = 0
        countLine line 
            | "XMAS" `isPrefixOf` line = 1 + countLine (drop 3 line)
            | "SAMX" `isPrefixOf` line = 1 + countLine (drop 3 line)
            | otherwise = countLine $ tail line

-- count all vertical "XMAS" in the contents
countVertical :: [String] -> Int
countVertical = countHorizontal . transpose

-- count all diagnal "XMAS" in the contents in all directions
countDiagnal :: [String] -> Int
countDiagnal content = 
    countDiagnalLowerRight content
    + countDiagnalLowerRight (reverse content) -- flip upside down
    + countDiagnalLowerRight (map reverse content) -- flip left and right
    + countDiagnalLowerRight (reverse (map reverse content)) -- flip in both directions

-- count all diagnal "XMAS" extending to LOWER-RIGHT
countDiagnalLowerRight :: [String] -> Int
countDiagnalLowerRight matrix =
    let rows = length matrix
        cols = if null matrix then 0 else length (head matrix)
        xmas = "XMAS"
        wordLength = length xmas
    in sum [ 1
           | i <- [0 .. rows - wordLength]
           , j <- [0 .. cols - wordLength]
           , isXmasDiagonal i j matrix xmas
           ]
        where
            -- check if there's a diagnal "XMAS" (or any specific word) starting from (i, j) in the matrix
            isXmasDiagonal :: Int -> Int -> [[Char]] -> String -> Bool
            isXmasDiagonal i j matrix word =
                and [ matrix !! (i + k) !! (j + k) == word !! k | k <- [0 .. length word - 1] ]

solutionQ1 :: IO ()
solutionQ1 = do
    horizontalInput <- inputHorizontalFromTxt "input.txt"
    let result = 
            countHorizontal horizontalInput
            + countVertical horizontalInput
            + countDiagnal horizontalInput
    print result

countXMASMMUp :: [String] -> Int
countXMASMMUp matrix =
    let 
        rows = length matrix
        cols = if null matrix then 0 else length $ head matrix
        mas  = "MAS"
        wordLength = length mas
    in  sum [ 1
            | i <- [0 .. rows - wordLength]
            , j <- [0 .. cols - wordLength]
            , isMas i j matrix mas]
        where
            isMas :: Int -> Int -> [String] -> String -> Bool
            isMas i j matrix word = 
                (and [matrix !! (i + k) !! (j + k) == word !! k | k <- [0 .. length word - 1]])
                && and [matrix !! (i + k) !! (j + 2 - k) == word !! k | k <- [0 .. length word - 1]]

countXMAS :: [String] -> Int
countXMAS matrix =
    countXMASMMUp matrix -- MM up
    + countXMASMMUp (transpose matrix) -- MM left
    + countXMASMMUp (reverse matrix) -- MM down
    + countXMASMMUp (reverse . transpose $ matrix) -- MM right

solutionQ2 :: IO ()
solutionQ2 = do
    horizontalInput <- inputHorizontalFromTxt "input.txt"
    let result = countXMAS horizontalInput
    print result

