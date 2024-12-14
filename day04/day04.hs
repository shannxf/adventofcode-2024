module Day04 where

inputFromTxt :: FilePath -> IO [String]
inputFromTxt filePath = do
    contents <- readFile filePath
    return $ lines contents

