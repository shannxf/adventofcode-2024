-- https://adventofcode.com/2024/day/3
module Day03 where
import Data.Char (isDigit)
import Data.List (isPrefixOf)

-- read contents into lists of string from the input file
inputFromTxt :: FilePath ->  IO String
inputFromTxt = readFile

-- define the Mul data type to facilitate evaluation and storing
data Mul = MkMul Int Int

eval :: Mul -> Int
eval (MkMul x y) = x * y

-- check if a string starts with a specific prefix
stripPrefix :: String -> String -> Maybe String
stripPrefix prefix s
    | prefix `isPrefixOf` s = Just (drop (length prefix) s) 
    | otherwise = Nothing

-- get the potential operands
parseMul :: String -> [Mul]
parseMul s =
    case span isDigit s of
        (x, ',':rest1) ->
            case span isDigit rest1 of
                (y, ')':rest2) -> [MkMul (read x) (read y)]
                _              -> []
        _ -> []

-- extract Mul data out of each line
extractMulPatterns :: String -> [Mul]
extractMulPatterns = go
  where
    -- recursively parsing the string
    go :: String -> [Mul]
    go [] = []
    go str =
        case stripPrefix "mul(" str of
            Just rest -> parseMul rest ++ go (drop 4 str) -- next "mul(" is only possible to start from the position after 4
            Nothing   -> go (tail str)

solutionQ1 :: IO ()
solutionQ1 = do
    contents <- inputFromTxt "input.txt"
    let result = sum . map eval . extractMulPatterns $ contents
    print result

-- extract Mul data out of each line
extractMulPatternsWithIgnore :: String -> [Mul]
extractMulPatternsWithIgnore str = go str True
  where
    -- recursively parsing the string
    go :: String -> Bool -> [Mul]
    go [] _ = []
    go str perform
        | "don't()" `isPrefixOf` str = go (drop 7 str) False
        | "do()"    `isPrefixOf` str = go (drop 4 str) True
        | not perform = go (tail str) False
        | otherwise = 
            case stripPrefix "mul(" str of
                Just rest -> parseMul rest ++ go (drop 4 str) perform -- next "mul(" is only possible to start from the position after 4
                Nothing   -> go (tail str) perform

solutionQ2 :: IO ()
solutionQ2 = do
    fileLines <- inputFromTxt "input.txt"
    let result = sum . map eval . extractMulPatternsWithIgnore $ fileLines
    print result