import Data.List
import Data.Char

--Exercise 1
subtotal :: Num a => [a] -> [a]
subtotal [] = [0]
subtotal xs = [sum tmp | z <- [0..((length xs)-1)], tmp <-[take (z+1) xs]]

--Exercise 2
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

histogram :: Int -> [Int] -> [Int]
histogram n [] = [0]
histogram 0 _ = [0]
histogram x xs = [length tmp| d <- [0..(((maximum xs) `div` x))],(c,b) <-[(x*d,x*(d+1))], tmp<-[[y | y<-quicksort xs, y <b, y >=c]]]

--Exercise 3
meetsOffer :: [Char] -> Int -> Bool
meetsOffer str n = n== sum[k| y <-str, (x,k)<-tuple, x==y]
  where tuple = [('*',8),('E',16),('D',24),('C',32),('B',40),('A',48)]

--Exercise 4
data TypeOfSort = Ascending | NonDescending | Constant | NonAscending | Descending | NotSorted
     deriving (Show)
comp :: Ord a => (a->a->Bool) -> [a] -> Bool
comp op [] = True
comp op [a] = True
comp op (a:b:[]) = (op) a b
comp op (a:b:xs) = (&&) ((op) a b) (comp op (b:xs))
sortType  ::   Ord a => [a]   -> TypeOfSort
sortType [] = Ascending
sortType (x:[]) = Ascending
sortType xs | comp (==) xs = Constant
            | comp (<) xs = Ascending
            | comp (>) xs = Descending
            | comp (<=) xs = NonAscending
            | comp (>=) xs = NonDescending
            | otherwise = NotSorted


--Exercise 5
rpcalc :: String -> Float
rpcalc  = head . foldl folding [] .words . addSpace
  where folding (x:y:ys) "+" = (y + x):ys
        folding (x:y:ys) "-" = (y - x):ys
        folding (x:y:ys) "*" = (y * x):ys
        folding (x:y:ys) "/" = (y / x):ys
        folding xs number = read number:xs

addSpace :: String -> String
addSpace = intersperse ' '

--Exercise 6
neighbours :: (Floating a, Ord a)=>Int->(a,a)->[(a,a)]->[(a,a)]
neighbours k p xs = take k (fst $ unzip sorted)
           where
                 distance = [sqrt((fst p - fst x)**2+(snd p - snd x)**2) | x <- xs]
                 zs = zip xs distance
                 sorted = sortBy (compare `on`  snd) zs

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(.*.) `on` f = \x y -> f x .*. f y

--Exercise 7
data SearchTree = Node SearchTree Int SearchTree | Leaf Int
     deriving Show

balanced :: SearchTree -> Bool
balanced (Leaf t) = True
balanced (Node l n r) = balanced l && balanced r && (strictlyI (>) n l) && (strictlyI (<) n r) -- && (difference l r)

--check if the tree in strictly increasing
strictlyI :: (Int -> Int-> Bool) -> Int -> SearchTree -> Bool
strictlyI op n (Leaf t) = (op) n t
strictlyI op n (Node l t r) = ((op) n t) && (strictlyI op n l) && (strictlyI op n r)

--check difference between heights
difference :: SearchTree -> SearchTree -> Bool
difference l r = abs ((-)(getMid l) (getMid r)) >= 1
getMid :: SearchTree -> Int
getMid (Leaf t) = t
getMid (Node l n r) = n

--Exercise 8
--This is an infinte seuquence and so it doesn't compute to print out
--I'm not sure how you are testing it, but what I have here is the code for the inifite sequence
--followed by the code for a sequence that stops once the root is reached as we both know the newton raphson method
--stops producing new numbers in the sequence once the root is reached

newtonRootSequence :: Double -> [Double]
newtonRootSequence d = newtonSequence d [1]

newtonSequence :: Double -> [Double] -> [Double]
--Newton Sequence that continues infintely:
--newtonSequence d xs = newtonSequence d ((++) xs [next])
--                   where last = head(reverse xs)
--                         next = (/)(last + (d /last)) 2

--Newton Sequence that stops when root is found (when elements in the sequence stop changing)
newtonSequence d xs | last == (next) = (++) xs [next]
                    | otherwise= newtonSequence d ((++) xs [next])
                   where last = head(reverse xs)
                         next = (/)(last + (d /last)) 2

newtonRoot::  Double ->  Double ->  Double
newtonRoot d e = head [a |(b,a) <- zip (list) (tail(list)), abs (a-b) < e]
           where list = newtonRootSequence d


--Exercise 9
hyperOperator :: Int-> Int-> Int-> Int
hyperOperator 0 a b = b+1
hyperOperator 1 a 0 = a
hyperOperator 2 a 0 = 0
hyperOperator 3 0 0 = 0
hyperOperator n a 0 = 1
hyperOperator 1 a b = a + b
hyperOperator 2 a b = a * b
hyperOperator 3 a b = (^) a b
hyperOperator 4 a b = expo a a b

expo :: Int -> Int -> Int -> Int
expo a c 1 = c
expo a c b = expo a (c^a) (b-1)

--Exercise 10
encode ::String -> [Int]
encode str = append[bits(addZero(decToBin  (ord c))) | c <-str]

bits :: [Int]-> [Int]
bits xs |(mod)(sum xs) 2 == 0 = xs++[0]
          | otherwise = xs ++ [1]

append :: [[Int]] -> [Int]
append [] = []
append (a:xs) = a ++(append xs)

decToBin::Int -> [Int]
decToBin 1 = [1]
decToBin 0 = [0]
decToBin c = (decToBin ((div) c 2))++[(mod) c 2]

addZero :: [Int] -> [Int]
addZero xs | (length xs) /= 8 = addZero ([0]++xs)
           | otherwise = xs

--Exercise 11
decode :: [Int] -> [Char]
decode xs | ((mod) (length xs) 9 /= 0) || (beg == "") || ((mod)(sum xs) 2 ==1) = ""
          | (length chunky) /= ((div) (length xs) 9)      = ""
          |otherwise = chunky
       where list = divideList xs
             beg = binToChar (head list)
             chunky = beg ++ decode(drop 9 xs)

binToChar:: [Int] -> [Char]
binToChar xs | ((mod)(sum xs) 2 == 0) && (length xs == 9) =  [chr ((2^7)*(xs!!0) + (2^6)*(xs!!1) + (2^5)*(xs!!2) + (2^4)*(xs!!3) + (2^3)*(xs!!4) + (2^2)*(xs!!5) + 2*(xs!!6) + 1*(xs!!7))]
             | otherwise = ""

divideList :: [Int] -> [[Int]]
divideList xs | length xs >9 = [fst (genericSplitAt 9 xs)] ++ divideList(snd( genericSplitAt 9 xs))
           | otherwise = [xs]

--Exercise 12
numTimesFound :: Ord a => a -> [a] -> Int
numTimesFound _ [] = 0
numTimesFound x list = sum $ map (\a -> 1) $ filter (== x) list

makeChange :: Int -> [Int] -> [Int]
makeChange 0 denoms = [0| x <- denoms]
makeChange _ [] = []
makeChange m denoms | sum coinUsed /= 0 = [numTimesFound x (coinUsed)| x <- denoms]
                    | otherwise = [-1 | x <- denoms]
                                where coinUsed = makeChange2 m denoms []

makeChange2 :: Int -> [Int] -> [Int] -> [Int]
makeChange2 0 denoms coinUsed = coinUsed

makeChange2 m denoms coinUsed | m >= head(sort denoms) = makeChange2 (fst(subs m denoms)) denoms coinUsed++[(snd(subs m denoms))]
                              | otherwise = [0]

subs :: Int -> [Int] -> (Int,Int)
subs m [] = (m,0)
subs m (x:denoms) | x <= m = (m-x,x)
                  | otherwise = subs m denoms


--Exercise 13
goodsteinSequence :: (Int, [Int]) -> [(Int, [Int])]
goodsteinSequence (b,[]) = [(b,[])]
goodsteinSequence (b,xs)= [(b,xs)]++goodsteinSequence(intToTuple (b+1) (tupleToInt (b,xs)))

tupleToInt :: (Int, [Int]) -> Int
tupleToInt (b,xs) = (sum [((b+1)^y)*x | (x,y)<-zip xs [0..]]) -1
intToTuple :: Int -> Int -> (Int,[Int])
intToTuple b 0 = (b,[])
intToTuple b i =  (b,[(mod) i b]++snd(intToTuple b ((div) i b)))

--Exercise 14
--Definitions of type Subst nad data Prop are from the lecture.
data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop
type Subst = [(Char, Bool)]
isSat :: Prop -> [Subst]

isSat (Const b) | b = [[]]
                | otherwise = []

isSat (Var c) = [[(c,True)]]

isSat (Not (Const b)) | b = []
                      |otherwise = [[]]

isSat (Not p) = [ not2 ps |ps<-(isSat (p))]
isSat (And (Const t) (Const f)) = isSat (Const (t && f))
isSat (And p1 (Const t)) | t = isSat p1
                         | otherwise = []
isSat (And (Const t) p1) = isSat (And p1 (Const t))
isSat (And p1 p2) = [and2 (p1s++p2s)|p1s <- (isSat p1) ,p2s <- (isSat p2)]

isSat (Imply (Const t) (Const f)) | (t && f) || (not t && not f) || (not t && f) = [[]]
                                  | otherwise = []
isSat (Imply (Const t) p1) | not t = (isSat p1)++(isSat (Not p1))
                           | otherwise = isSat p1
isSat (Imply p1 (Const t)) = isSat (Imply (Not (Const t)) (Not p1))
isSat (Imply p1 p2) = [imply (p1s++p2s){-, imply (p1s++pn2s),imply (pn1s++pn2s), imply (pn1s++p2s)-} | p1s <- isSat p1, p2s <- isSat p2{-, pn1s <- isSat (Not p1), pn2s <- isSat (Not p2)-}]

and2, imply :: [(Char, Bool)] -> [(Char ,Bool)]
and2 xs = removeDuplicates [(a,b)|(a,b)<-xs, (c,d) <- xs, a/=c, b && d]
imply xs = removeDuplicates [(a,b) | (a,b) <- xs, (c,d)<-xs, a/=c, b<=d]
not2 xs = removeDuplicates [(a, c) | (a, b) <-xs, c <- [True,False], c==not b]
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs


--Exercise 15
xysum :: Integer -> Integer -> Integer
xysum  x y = x + y

pair :: Integer -> Integer -> Integer
pair x y = y + ((xysum x y) * ((xysum x y)+ 1)) `div` 2

isCantorPair :: Integer ->Bool{- -> Integer -> Integer -> Integer -}-- -> Bool
isCantorPair  x = x == y
                where   cantorList = getCantorPair x
                        cantorPairX = sort $ getCantorPair  $ head $ tail $ sort cantorList
                        cantorFirstX = cantorPairX !! 0
                        cantorSecondX = cantorPairX !! 1
                        y = pair (pair cantorFirstX cantorSecondX) (xysum  cantorFirstX cantorSecondX)

getCantorPair :: Integer -> [Integer]
getCantorPair x = [cantorFirst x, cantorSecond x]

cantorFirst :: Integer -> Integer --y
cantorFirst x = x - (t x)

cantorSecond :: Integer -> Integer --x
cantorSecond x = (sumIt x) - (cantorFirst x)

t::Integer -> Integer
t x = ((div)(((sumIt x) + 1) * (sumIt x)) 2)

sumIt :: Integer -> Integer --w= floor (((sqrt(8z+1))-1)/2)
sumIt x = floor ((/) ((-) (sqrt (8*(fromInteger x) + 1)) 1) 2)
