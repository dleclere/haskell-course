module Cypher1B where
  
import Data.Char
import Common
import Data.List
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map.Strict as Map



letterFreqs = Map.map (/100.0) ((Map.fromList [
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

rankKeyOptions :: [String] -> String -> [(String -> Double)] -> [String]
rankKeyOptions keys cypher fns = map (fst) $ sortBy (cmpDesc snd) ranks
  where 
    ranks = zip keys $ map (\k -> scoreKey k cypher fns) keys

scoreKey :: String -> String -> [(String -> Double)] -> Double
scoreKey k cypher fns = 
  foldl (+) 0.0 (map (\f -> f dc) fns)
  where
    dc = decode k cypher

-- This is a little toy implementation of the Vigenère Cipher - written primarly for autodidactic purposes. The code is based on the Caesar Cipher code of Graham Hutton's book Programming in Haskell. This post is written in literal Haskell, so you should be able to copy paste the whole thing into an *.lhs file and run it.

-- First the encryption and decryption functions:

-- converts letter (ASCII encoded) to numeric value
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- converts a numeric value to ASCII
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

-- shift letter by n places modulo 26
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c


-- The Caesar and Vigenère ciphers are closely realted. In a Caesar cipher, each letter of the alphabet is shifted along some number of places. For example, in a Caesar cipher of shift 3, A would become D, B would become E and so on. This is what the shift function is used for. The Vigenère cipher consists of several Caesar ciphers in sequence with different shift values. I used a list comprehension to cycle through the provided key shifting each letter as required.

-- vigenere encodes the message
encode :: String -> String -> [Char]
encode key msg = [ shift (let2int k) c | (k,c) <- zip (cycle key) msg]

-- The decode function is just the inverse of the encode function. On a sidenote; this would be a nice property to test with quickcheck.

-- vigenere decodes the message
decode :: String -> String -> [Char]
decode key msg = [ shift (26 - let2int k) c | (k,c) <- zip (cycle key) msg]

-- This takes care of the cryptographic functions in the characteristic concise haskelly way. But the intersting part is the cryptoanalytical part of cracking the cipher. To crack Vigenère, you first have to figure out the length of the key used to encrypt the plain text. There are actually two methods to do this. Either using a statistical method called the Index of Coincidence (IC) or a more mechanial approach called Kasiski's Method. I've only implemented the first approach here, mainly because I'm lazy. Kasiski's method is actually easier to understand and probably more accurate for longer keys than is IC...
-- Once you know the cipher key length, you can separate the underlying Caesar Cihpers and perform perform individual frequency analysis - one for each position in the key. The last step is then to match the observed frequencies with a table of expected frequencies. This allows you to calculate the key.

{- Frequency analysis -}
type FrequencyTable = [Float]

-- frequency table for the english language (expected values)
table :: FrequencyTable
table =  [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0,
          6.1, 7.0, 0.2, 0.8,  4.0, 2.4, 6.7,
          7.5, 1.9, 0.1, 6.0,  6.3, 9.1, 2.8,
          1.0, 2.4, 0.2, 2.0,  0.1]

lowers :: String -> Int
lowers xs =  length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs =  length [x' | x' <- xs, x == x']

percent :: Int -> Int -> Float
percent n m =  (fromIntegral n / fromIntegral m) * 100

-- Freqs calculates the observed frequencies (in percent) of all the characters in the cipher text. Frequency analysis is a very common attack on many pen an paper ciphers.

freqs :: String -> [Float]
freqs xs =  [percent (count x xs) n | x <- ['a'..'z']]
            where n = lowers xs


indexOfCoincidence :: String -> Float
indexOfCoincidence xs = 
  fromIntegral (sum [f * (f-1) | f <- counts]) / fromIntegral (n * (n-1)) 
    where
      counts = [(count x xs) | x <- ['a'..'z']]
      n = sum counts

-- The function passwordLength deduces the likely password length for english texts based on the Index of Coincidence. This is sometimes also called the Friedman test. There's a lot of statistical magic going on behind the scenes and the details are beyond the scope of this text. As I mentioned earlier IC suffers from some weaknesses, first and foremost it gets inaccurate the longer the cipher key is.

passwordLength :: String -> Int
passwordLength xs | ic  > 0.06552 =  0
                  | ic  > 0.05185 =  1
                  | ic  > 0.04730 =  2
                  | ic  > 0.04502 =  3
                  | ic  > 0.04365 =  4
                  | ic  > 0.04274 =  5
                  | ic  > 0.04209 =  6
                  | ic  > 0.04160 =  7
                  | ic  > 0.04122 =  8
                  | ic  > 0.04092 =  9
                  | otherwise     = 13
                  where 
                     ic = indexOfCoincidence xs

-- Once you know length of the key, you can proceed to calculate the frequencies for each cipher key character. To find the best match between the expected frequencies in the FrequencyTable and the observed frequencies in the cipher text, a statistical method called Chi Square Test is used. 

-- Chi square for the observed frequencies
chisqr :: [Float] -> [Float] -> Float
chisqr os es =  sum [((o - e) ^ 2) / e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs =  drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs =  [i | (x',i) <- zip xs [0..], x == x']


-- Bestmatch cycles through the all possible shifts (zero to 25) and returns the most likely number of positions the alphabet was shifted, i.e. the one with the lowest Chi-Square value.

bestmatch :: String -> Int
bestmatch xs = head (positions (minimum chitab) chitab)
               where 
                  chitab = [ chisqr (rotate n table') table | n <- [0..25]]
                  table' = freqs xs
                  
bestmatches :: String -> [Int]
bestmatches xs = best
               where
                  best = flatMap chipos $ take 4 $ sort chitab
                  chipos ct = positions ct chitab
                  chitab = [ chisqr (rotate n table') table | n <- [0..25]]
                  table' = freqs xs
                  


separateAlphabets :: String -> Int -> [String]
separateAlphabets xs n = [[ c | (c,p) <- zip xs (cycle [0..n-1]), p `mod` n == i] | i <- [0..n-1]]

findKey :: String -> String
findKey msg = [ int2let (bestmatch s) | s <- alphas ]
              where
                 alphas = separateAlphabets msg cnt
                 cnt = passwordLength msg
                 
findKeys :: Int -> String -> [String]
findKeys klen msg = build 0 4
  where 
      matches = [ map (int2let) (bestmatches s) | s <- alphas ]
      alphas = separateAlphabets msg klen
      ranked = matches
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
                children = take ((klen - ci) + d) $ build (ci + 1) d

-- The crack function combines all of the above into another Haskell one-liner:

crack :: String -> String
crack cipherText = decode (findKey cipherText) cipherText 

