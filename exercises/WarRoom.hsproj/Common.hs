module Common where
  
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List
import Data.Char
  

toNum :: Char -> Int
toNum c = (ord c) - 65

toChar :: Int -> Char
toChar n = chr (n + 65)

findSubstring :: Eq a => [a] -> [a] -> Maybe Int
findSubstring pat str = findIndex (isPrefixOf pat) (tails str) 

mergeLines :: [String] -> String
mergeLines lines = foldl (++) "" $ map (++ " ") lines

stripSpaces :: String -> String
stripSpaces text = foldl (++) "" (words text)

stripChars :: String -> String -> String
stripChars = filter . flip notElem

keepChars :: String -> String -> String
keepChars = filter . flip elem

stripPunct :: String -> String
stripPunct str = keepChars whitelist str
  where whitelist = [' '] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  
spacePunct :: String -> String
spacePunct str = map (rpl) str
  where 
    rpl c
      | c `notElem` whitelist = ' '
      | otherwise = c
        where whitelist = [' '] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
        
characterFrequency :: String -> (Map Char Integer)
characterFrequency str = foldl (add) Map.empty str
  where 
    add m c = Map.insert c count m
       where count = (fromMaybe 0 $ Map.lookup c m) + 1
     
ic :: String -> Double  
ic str = (Map.foldl (\m x -> m + (cProb x)) 0.0 $ characterFrequency sstr) / (tn / 26)
  where
    sstr = stripSpaces str
    strLen = length sstr
    tn = fromInteger $ (toInteger $ strLen * (strLen - 1)) :: Double
    cProb cCount = fromInteger (cCount * (cCount - 1)) :: Double

countIC :: String -> String -> Double
countIC a b = (count a b) / ((fromInteger (toInteger $ (length a)) :: Double) / 26.0)
  where 
    count "" _ = 0.0
    count _ "" = 0.0
    count (x:xs) (y:ys)
      | x == y = 1.0 + next
      | otherwise = next
        where next = count xs ys
        
shiftText :: String -> Int -> String
shiftText text offset = map (toChar . (+offset) . toNum) text

friedmanTestEnglish str = (kP - kR) / (kO - kR)
  where
    kP = 0.067
    kR = 0.0385
    kO = ic str
    
flatMap _ [] = []  
flatMap f (x:xs) = f x ++ flatMap f xs

flatten [] = []
flatten (x:xs) = x ++ (flatten xs)

factors n = [x | x <- [1..n], n `mod` x == 0]

frequency :: Ord a => [a] -> [(Int,a)] 
frequency list = map (\l -> (length l, head l)) (group (sort list))


cmpDesc f a b
  | a' < b' = GT
  | a' > b' = LT
  | a' == b' = EQ
  where 
    a' = f a
    b' = f b
    
cmpAsc f a b
  | a' > b' = GT
  | a' < b' = LT
  | a' == b' = EQ
  where 
    a' = f a
    b' = f b
    
every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []

