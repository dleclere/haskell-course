module Cypher1 where
  
import Common

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Char
import Data.List.Split
import Data.List
import System.Environment
import System.IO


letterFreqs = Map.map (/100.0) (Map.mapKeys (toUpper) (Map.fromList [
  ('e',12.702),
  ('t',9.056),
  ('a',8.167),    
  ('o',7.507),    
  ('i',6.966),    
  ('n',6.749),    
  ('s',6.327),    
  ('h',6.094),    
  ('r',5.987),    
  ('d',4.253),    
  ('l',4.025),    
  ('c',2.782),    
  ('u',2.758),    
  ('m',2.406),    
  ('w',2.361),    
  ('f',2.228),    
  ('g',2.015),    
  ('y',1.974),    
  ('p',1.929),    
  ('b',1.492),    
  ('v',0.978),    
  ('k',0.772),    
  ('j',0.153),
  ('x',0.150),
  ('q',0.095),
  ('z',0.074)
  ]))
  
bigramFreqs = Map.map (/100.0)  (Map.fromList[
  ("TH",3.56),
  ("HE",3.07),
  ("IN",2.43),
  ("ER",2.05),
  ("AN",1.99),
  ("RE",1.85),
  ("ON",1.76),
  ("AT",1.49),
  ("EN",1.45),
  ("ND",1.35),
  ("TI",1.34),
  ("ES",1.34),
  ("OR",1.28),
  ("TE",1.20),
  ("OF",1.17),
  ("ED",1.17),
  ("IS",1.13),
  ("IT",1.12),
  ("AL",1.09),
  ("AR",1.07),
  ("ST",1.05),
  ("TO",1.04),
  ("NT",1.04),
  ("NG",0.95),
  ("SE",0.93),
  ("HA",0.93),
  ("AS",0.87),
  ("OU",0.87),
  ("IO",0.83),
  ("LE",0.83),
  ("VE",0.83),
  ("CO",0.79),
  ("ME",0.79),
  ("DE",0.76),
  ("HI",0.76),
  ("RI",0.73),
  ("RO",0.73),
  ("IC",0.70),
  ("NE",0.69),
  ("EA",0.69),
  ("RA",0.69),
  ("CE",0.65),
  ("LI",0.62),
  ("CH",0.60),
  ("LL",0.58),
  ("BE",0.58),
  ("MA",0.57),
  ("SI",0.55),
  ("OM",0.55),
  ("UR",0.54)
  ])
  
octGrams = map (\cs -> map toUpper cs) [
  "tion",
  "atio",
  "that",
  "ther",
  "with",
  "ment",
  "ions",
  "this",
  "here",
  "from",
  "ould",
  "ting",
  "hich",
  "whic",
  "ctio",
  "ence",
  "have",
  "othe",
  "ight",
  "sion",
  "ever",
  "ical",
  "they",
  "inte",
  "ough",
  "ance",
  "were",
  "tive",
  "over",
  "ding",
  "pres",
  "nter",
  "comp",
  "able",
  "heir",
  "thei",
  "ally",
  "ated",
  "ring",
  "ture",
  "cont",
  "ents",
  "cons",
  "rati",
  "thin",
  "part",
  "form",
  "ning",
  "ecti",
  "some"]
  
triGrams = map (\cs -> map toUpper cs) [
  "the",
  "and",
  "ing",
  "ion",
  "tio",
  "ent",
  "ati",
  "for",
  "her",
  "ter",
  "hat",
  "tha",
  "ere",
  "ate",
  "his",
  "con",
  "res",
  "ver",
  "all",
  "ons",
  "nce",
  "men",
  "ith",
  "ted",
  "ers",
  "pro",
  "thi",
  "wit",
  "are",
  "ess",
  "not",
  "ive",
  "was",
  "ect",
  "rea",
  "com",
  "eve",
  "per",
  "int",
  "est",
  "sta",
  "cti",
  "ica",
  "ist",
  "ear",
  "ain",
  "one",
  "our",
  "iti",
  "rat"
  ]
  
bigrams = Map.keys bigramFreqs

encrypt key str = map (ec) tpls
  where
    tpls = zip [0..] str
    keyLen = length key
    charAt i = key !! (i `mod` keyLen)
    ec (i, c) = encryptChar (charAt i) c
    
decrypt key str = map (dc) tpls
  where
    tpls = zip [0..] str
    keyLen = length key
    charAt i = key !! (i `mod` keyLen)
    dc (i, c) = decryptChar (charAt i) c

encryptChar k c = toChar $ ((toNum c) + toNum k) `mod` 26

decryptChar k c = toChar $ ((toNum c) - toNum k) `mod` 26

keyChar plain crypt = toChar $ ((toNum crypt) - (toNum plain)) `mod` 26

estLen :: String -> [Int] -> [(Int, Double)]
estLen cypher keyRange = oTpls
  where
    oTpls = sortBy cmp (filter (\x -> (snd x) > 0) ciTpls)
      where 
        cmp (_, i1) (_, i2)
          | i1 < i2 = GT
          | i1 > i2 = LT
          | i1 == i2 = EQ
    ciTpls = map (\x -> (x, avgFt $ chunksOf x cypher)) keyRange
    avgFt :: [String] -> Double
    avgFt rows = (foldl (\m x -> m + (ic x)) 0.0 rows) / rc
      where rc = fromInteger (toInteger $ length rows) :: Double
      
rankKeyOptions :: [String] -> String -> [(String -> Double)] -> [String]
rankKeyOptions keys cypher fns = map (fst) $ sortBy (cmpDesc snd) ranks
  where 
    ranks = zip keys $ map (\k -> scoreKey k cypher fns) keys

scoreKey :: String -> String -> [(String -> Double)] -> Double
scoreKey k cypher fns = 
  foldl (+) 0.0 (map (\f -> f dc) fns)
  where
    dc = decrypt k cypher

rankSubOptions subs cypher = sortBy (cmpDesc snd) ranks
  where 
    ranks = zip subs $ map (`scoreSub` cypher) subs

scoreSub s cypher = letterCorrelationScore dc
  where
    dc = map (decryptChar s) cypher

letterCorrelationScore str = Map.foldlWithKey (build) 0.0 freq
  where
    freq = characterFrequency str
    build memo chr count = memo + ((fromInteger count :: Double) * (letterFreqs Map.! chr))
    
letterCorrelationScore2 str =
  foldr (+) 0.0 $ (map (letterFreqs Map.!) str)
  
      
simpleTrigramScore :: String -> Double
simpleTrigramScore cypher = fromInteger (toInteger $ length $ filter (`isInfixOf` cypher) bigrams) :: Double
      
bigramCorrelationScore2 cypher = process 0.0 cypher
  where
    process :: Double -> String -> Double
    process m "" = m
    process m str = m + (process (freq (take 2 str)) (drop 1 str))
    freq str = fromMaybe 0.0 $ Map.lookup str bigramFreqs

columnFreq rowLen cypher =  map ((sortBy cmp) . Map.toList . characterFrequency) cols
  where 
    cols = splitToCols rowLen cypher
    cmp (_, i1) (_, i2)
      | i1 < i2 = GT
      | i1 > i2 = LT
      | i1 == i2 = EQ
      

    
bigramCorrelationScore str = Map.foldlWithKey (build) 0.0 freq
  where 
    freq = Map.map (toInteger . length) (findNGrams 2 str)
    build memo bi count
      | Map.member bi bigramFreqs = memo + ((fromInteger count :: Double) * (bigramFreqs Map.! bi))
      | otherwise = memo

aggregateColICMulti rowLens cypher = 
  sortBy (cmp) tpls
  where
    tpls = zip rowLens $ map (`aggregateColIC` cypher) rowLens
    cmp (_, i1) (_, i2)
      | i1 < i2 = GT
      | i1 > i2 = LT
      | i1 == i2 = EQ  

aggregateColIC rowLen cypher = sum / fromInteger (toInteger $ length cols) :: Double
  where
    cols = splitToCols rowLen cypher
    colIcs = map (ic) cols
    sum = foldl (+) 0.0 colIcs

splitToCols rowLen cypher = cols
  where 
    rows = chunksOf rowLen cypher
    cols = map (\c -> foldl (\m r -> m ++ [colChar r c]) "" rows) [0..(rowLen - 1)]
    colChar row col
      | col < length row = row !! col
      | otherwise = ' '
      
estimateLenNGramReps cypher = sortBy (cmp) facts
  where
    facts = frequency $ flatMap (factors) $ flatten dists
    dists = Map.elems $ findRepeatedNGramDistanceMulti [2..8] cypher
    cmp (i1, _) (i2, _)
      | i1 < i2 = GT
      | i1 > i2 = LT
      | i1 == i2 = EQ

findRepeatedNGramDistanceMulti ns cypher =
  Map.fromList $ flatMap (Map.toList) $ map (`findRepeatedNGramDistance` cypher) ns 
  

findRepeatedNGramDistance n cypher = distances
  where
    distances = Map.map (distance) $ findRepeatedNGrams n cypher
    distance [] = []
    distance (x:y:xs) = (x - y) : (distance xs)
    distance (x:xs) = []
--    inverted = Map.foldlWithKey (add) Map.empty distances 
--    add :: (Map Int [String]) -> String -> [Int] -> Map Int [String]
--    add m str is = foldl (subAdd) m is
--      where 
--        subAdd sm i = Map.insert i strs sm
--          where
--            strs = str:(fromMaybe [] $ Map.lookup i m)
    

findRepeatedNGrams n cypher = Map.filter (\is -> (length is) > 1) $ findNGrams n cypher
      
findNGrams n cypher = process Map.empty cypher 0
  where
    process :: (Map String [Int]) -> String -> Int -> (Map String [Int])
    process m "" _ = m
    process m str i = process (add m (take n str) i) (drop 1 str) (i + 1)
    add :: (Map String [Int]) -> String -> Int -> Map String [Int]
    add m str i = Map.insert str indices m
      where indices = i:(fromMaybe [] $ Map.lookup str m)

estLenIC guessLens str = sortBy (cmp) $ zip guessLens ics
  where 
    ics = map ((countIC str) . (str `shiftText`)) guessLens
    cmp (_, i1) (_, i2)
      | i1 < i2 = GT
      | i1 > i2 = LT
      | i1 == i2 = EQ  
    


elimKey keyLen cypher = decrypt offset cypher
  where offset = (drop keyLen cypher) ++ (take keyLen cypher)
  
substitute subs str = map (sub) str
  where 
    sub c = (fromMaybe c $ Map.lookup c subs)
    
substituteRows subs rows = map (substitute subs) rows

--printRows :: [String] -> (IO String)
printRows rows = putStr $ unlines rows

vowls = "AEIOU"
  
guessKey klen cypher rankers = rankKeyOptions (candidates) cypher rankers
  where
    filterKey key = hasVowl && hasBigram
      where 
        hasVowl = (length $ key `intersect` vowls) > 0
        hasBigram =  not $ null $ filter (`isInfixOf` key) bigrams
    candidates = filter filterKey $ build 0 ['A'..'Z']
    cols = splitToCols klen cypher
    ranked = map (map (fst) . rankSubOptions ['A'..'Z']) cols
    build :: Int -> [Char] -> [String]
    build ci range
      | ci == klen = []
      | otherwise = flatMap (apply) best
      where 
        best :: [Char]
        best = take 3 ((ranked!!ci) `intersect` range)
        apply :: Char -> [String]
        apply c 
          | children == [] = map (:[]) best
          | otherwise = map (c:) children
            where 
              children = build (ci + 1) (filter (\x -> x /= c) range)              

    
guessKey2 klen cypher rankers depth = rankKeyOptions (candidates) cypher rankers
  where
    filterKey key = hasVowl && hasBigram
      where 
        hasVowl = (length $ key `intersect` vowls) > 0
        hasBigram =  not $ null $ filter (`isInfixOf` key) bigrams
    --candidates = filter (\x -> simpleTrigramScore (decrypt x cypher) > 0) $ build 0
    candidates = filter (\x -> (simpleTrigramScore x) > 0 && (simpleTrigramScore (dc x) > 0)) $ build 0 depth
      where 
        cy = take (klen) cypher
        dc key = decrypt key cy
    cols = splitToCols klen cypher
    ranked = map (map (fst) . rankSubOptions ['A'..'Z']) cols
    build :: Int -> Int -> [String]
    build ci d
      | ci == klen = []
      | otherwise = flatMap (apply) best
      where 
        best :: [Char]
        best = take d (ranked!!ci)
        apply :: Char -> [String]
        apply c 
          | children == [] = [[c]]
          | otherwise = map (c:) children
            where 
              children = build (ci + 1) (d)             

guessKey3 klen cypher rankers depth = rankKeyOptions (candidates) cypher rankers
  where
    filterKey key = hasVowl
      where 
        hasVowl = (length $ key `intersect` vowls) > 0
    --candidates = filter (\x -> simpleTrigramScore (decrypt x cypher) > 0) $ build 0
    candidates = build 0 depth
      where 
        cy = take (klen) cypher
        dc key = decrypt key cy
    cols = splitToCols klen cypher
    ranked = map (map (fst) . rankSubOptions ['A'..'Z']) cols
    build :: Int -> Int -> [String]
    build ci d
      | ci == klen = []
      | otherwise = flatMap (apply) best
      where 
        best :: [Char]
        best = take d (ranked!!ci)
        apply :: Char -> [String]
        apply c 
          | children == [] = [[c]]
          | otherwise = map (c:) children
            where 
              children = build (ci + 1) (d) 

doWork words cypher klen = 
  take 20 $ rankKeyOptions keys cypher [letterCorrelationScore2]
    where 
      fkeys = filter (\x -> (simpleTrigramScore (dc1 x) > 0) && (simpleTrigramScore (dc2 x) > 0) && (simpleTrigramScore (dc3 x) > 0)) $ keys
        where 
          cy = take (klen) cypher
          cy2 = take (klen) $ drop klen cypher
          cy3 = take (klen) $ drop (klen * 2) cypher
          dc1 key = decrypt key cy
          dc2 key = decrypt key cy2
          dc3 key = decrypt key cy3
      keys = (map (\cs -> map toUpper cs) $ filter (\w -> (length w) == klen) $ lines words)
    
    
readWordList inp cypher wordCount = 
    do
      words <- readFile inp 
      putStr $ unlines $ doWork words cypher wordCount


