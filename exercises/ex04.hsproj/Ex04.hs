module Ex04 where

import Data.Char
import System.Environment
import System.IO

capitalise :: FilePath -> FilePath -> IO ()
capitalise inp outp = 
    do
      lowerCase <- readFile inp
      writeFile outp $ map toUpper lowerCase

str2Int :: String -> Int
str2Int str = read str :: Int

sumLines :: String -> Int
sumLines str = foldl (+) 0 ilns
  where 
    lns = lines str
    ilns = map str2Int lns

sumFile :: IO ()
sumFile =
    do
      [inp, outp] <- getArgs
      inf <- readFile $ inp
      writeFile outp $ show $ sumLines inf