{-# LANGUAGE FlexibleContexts #-}

module Codeword where

import Data.Foldable  
import Common


p1 = [
  "Are you interested in puzzles, strategy, attack and defence? At UNSW we have",
  "a strong focus on the fast-growing field of cyber security combining both a focus",
  "on advanced security theory with an emphasis on the mastery of highly technical",
  "cyber attack and defence skills."
  ]
  
p2 = [
  "We are known as the leading Australian university in cyber security training - we’ve won",
  "each of the Australian University Cyber Security Challenges since they started in 2012,",
  "and in 2013 we entered three teams and won First, Second and Third places.",
  "Have you got what it takes?"
  ]

p3 = [
  "Try out your analysis, observation, cunning and lateral thinking skills in the UNSW",
  "Computing Capture The Flag competition (or send us a letter)"
  ]

p4 = [
  "What is the code word?",
  "18 120 118 18 84 116 22 61 121"
  ]

cypher = p1 ++ p2 ++ p3 ++ p4
  

key :: [Int]
key = [18, 120, 118, 18, 84, 116, 21, 61, 121]

key2 = [1, 8, 1, 2, 0, 1, 1, 8, 1, 8, 8, 4, 1, 1, 6, 2, 2, 6, 1, 12, 1]

key3 = [[1,8], [1,2,0], [1,1,8], [1, 8], [8,4], [1,1,6], [2, 2], [6,1], [1,2,1]]

key4 :: [Int]
key4 = map (foldl (+) 0) key3 

key5 :: [Int]
key5 = map (foldl (*) 1) key3

key6 = foldl (\m x -> m ++ [x, x + 1, x +3]) [] key

key7 = [18, 121, 119, 18, 84, 117, 22, 61, 122]

  

attempt1 key = map (input!!) key
  where input = (stripPunct (mergeLines cypher))

attempt2 key = map (input!!) key
  where input = stripSpaces $ stripPunct (mergeLines cypher)
  
attempt3 key = foldl (++) "" $ map (++ ",") $ map (wrds!!) key
  where wrds = words $ spacePunct $ mergeLines cypher

attempt4 key = (map (\x -> take 3 x) kw)
  where 
    wrds = reverse $ words $ spacePunct $ mergeLines cypher
    kw = map (wrds!!) key

attempt5 key = zipWith (\k l -> l!!k) key lns
  where 
    lns = map (stripPunct) cypher
    
attempt6 key = zipWith (\k l -> l!!k) key lns
  where 
    lns = map (stripSpaces . stripPunct) cypher
    
attempt7 key = map (rKey) key
  where
    lns = map (stripSpaces . stripPunct) cypher
    rKey :: Int -> Char
    rKey k = c
      where 
        kl = (quot k 10) - 1
        kc = k `mod` 10
        c = (lns!!kl)!!kc
      
attempt8 key = map (rKey) key
  where
    lns = map (stripSpaces . stripPunct) cypher
    rKey :: Int -> Char
    rKey k = c
      where 
        kl = (quot k 10) - 1
        kc = k `mod` 10
        c = (lns!!kl)!!kc
        
attempt9 key = map (rKey) key
  where
    lns = fmap words $ map (stripSpaces . stripPunct) cypher
    rKey k = c
      where 
        kl = (quot k 10) - 1
        kc = k `mod` 10
        c = (lns!!kl)!!kc
        
attempt10 key = map (input!!) key
  where input = stripSpaces $ stripPunct (mergeLines $ reverse $ cypher)
  
attempt11 key = map (rKey) key
  where
    lns = map (stripSpaces . stripPunct) cypher
    rKey k = c
      where 
        col = (k `quot` 10)
        row = k `mod` 10
        c = (lns!!row)!!col
        
attempt12 key = map (rKey) key
  where
    lns = map (stripPunct) cypher
    rKey k = c
      where 
        col = (k `quot` 10)
        row = k `mod` 10
        c = (lns!!row)!!col
        
attempt13 key = map (rKey) key
  where
    lns = map (stripSpaces) cypher
    rKey k = c
      where 
        col = (k `quot` 10)
        row = k `mod` 10
        c = (lns!!row)!!col

attempt14 key = map (rKey) key
  where
    lns = cypher
    rKey k = c
      where 
        col = (k `quot` 10)
        row = k `mod` 10
        c = (lns!!row)!!col
        
attempt15 :: [Int] -> [String]
attempt15 key = map (rkey) key
  where
    lns = map (words) cypher
    rkey k = w
      where 
        col = (k `quot` 10)
        row = k `mod` 10
        w = (lns!!row)!!col
        

attempt16 = map (!!0) $ take 10 wrds
  where wrds = words $ spacePunct $ mergeLines p1
--attempt16 key = map (wrds!!) key
--  where wrds = 


attemptS1 key = foldl (++) "" $ zipWith (\ks l -> map (l!!) ks) key lns
  where 
    lns = map (stripSpaces . stripPunct) $ cypher
      