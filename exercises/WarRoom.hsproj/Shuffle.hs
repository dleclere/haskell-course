{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Shuffle where
  
import Text.Regex.TDFA  
import Text.Regex.TDFA.Text
import Data.Char
import System.Environment
import System.IO
import Data.List
import Common

ptn = "(?:\\\\|\\|/:)+:*([:digit:]+):*(?:\\\\|\\|/:)+"

lnNum :: String -> Int
lnNum str = read num :: Int
  where 
    pt = "[0-9]+" :: String
    num = (str =~ pt :: String)
    
rmNum :: String -> String
rmNum str = pre ++ post
  where 
    pre = take i str
    post = drop (i + l) str
    ptn = ":{1,2}[0-9]+:{1,2}" :: String
    (i, l) = str =~ ptn :: (MatchOffset,MatchLength)

orderLns :: [String] -> [String]
orderLns uo = ordered
  where
    ordered = map (snd) oTpls
    oTpls = sortBy cmp uoTpls 
      where 
        cmp (i1, _) (i2, _)
          | i1 < i2 = LT
          | i1 > i2 = GT
          | otherwise = EQ
    uoTpls = map tpl uo
      where tpl ln = ((lnNum ln), ln)


unshuffle inp = 
    do
      shuffled <- readFile inp
      putStr $ unlines $ map rmNum $ orderLns $ lines shuffled