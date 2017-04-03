doubleMe :: (Num a) => a -> a
doubleMe x = x * 2

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = (if x > 100 then x else x * 2) + 1

listComprehension :: (Ord a, Num a, Enum a) => [Int]
listComprehension = [x * 2 | x <- [1..10], x * 2 >= 12]

concatenateLists :: [a] -> [a] -> [a]
concatenateLists x y = x ++ y

getHead :: [a] -> a
getHead x = head x

getTail :: [a] -> [a]
getTail x = tail x

getLast :: [a] -> a
getLast x = last x

getInit :: [a] -> [a]
getInit x = init x

getLength :: [a] -> Int
getLength x = length x

isEmpty :: [Int] -> Bool
isEmpty x = null x

reverseList :: [Int] -> [Int]
reverseList x = reverse x

takeFirstN :: Int -> [Int] -> [Int]
takeFirstN n x = take n x

skipFirstN :: Int -> [Int] -> [Int]
skipFirstN n x = drop n x

getMax :: [Int] -> Int
getMax x = maximum x

getMin :: [Int] -> Int
getMin x = minimum x

sumAll :: [Int] -> Int
sumAll x = sum x

multiplyAll :: [Int] -> Int
multiplyAll x = product x

getNFromList :: Int -> [Int] -> Bool 
getNFromList n x = n `elem` x

makeRangeList :: Int -> Int -> [Int]
makeRangeList from to = [from..to]

makeStepRangeList :: Int -> Int -> Int -> [Int]
makeStepRangeList from to step = [from, from + step..to]

cycleList :: [Int] -> [Int]
cycleList x = cycle x

repeatX :: Int -> [Int]
repeatX x = repeat x

replicateNtimesX :: Int -> Int -> [Int]
replicateNtimesX n x = replicate n x

getMod7With3Remainder :: [Int]
getMod7With3Remainder = [ x | x <- [50..100], x `mod` 7 == 3]

boomBangs :: [Int] -> [String]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

getLength' :: [Int] -> Int
getLength' xs = sum [1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

getTupleFirst :: (a, b) -> a
getTupleFirst x = fst x

getTupleSecond :: (a, b) -> b
getTupleSecond x = snd x

combineLists :: [a] -> [b] -> [(a, b)]
combineLists x y = zip x y

rightTriangles :: (Num a, Enum a, Eq a) => [(a, a, a)]
rightTriangles = [(a, b, c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

-- TYPECLASSES
-- (typeclass typeVariable) => returnType
-- Eq: equality checks (==, /=)
-- Ord: element ordering (>, <, >=, <=)
-- Show: output value as String (show 3)
-- Read: takes Strings and return a Read type (read "5" :: Int)
-- Enum: sequentially enumerable ordered types, can use list ranges (['a'..'e''])
-- Bounded: member have an upper and lower bound (minBound :: Int)
-- Num: includes ALL numbers (type *)
-- Integral: includes Int and Integer
-- Floating: includes Float and Double

-- PATTERN MATCHING (order does count)
-- recursion by pattern matching
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)
-- catch-all pattern
-- result of a known input (edge condition)

-- GUARDS
-- WHERE
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname    

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^2
  in  sideArea + 2 * topArea

-- LET (let <bindings> in <expression>)
[if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"]

4 * (let a = 9 in a+1) + 2

[let square x = x * x in (square 5, square 3, square 2)]

(let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)

(let (a,b,c) = (1,2,3) in a+b+c) * 100

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi | (w,h) <- xs, let bmi = w / h^2]

case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."

describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."

-- RECURSION
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs) = max x (maximum' xs)  

replicate' :: (Num a, Ord a) => a -> b -> [b]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n-1) x

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]  
    biggerSorted = quicksort [a | a <- xs, a > x]  
  in smallerSorted ++ [x] ++ biggerSorted

-- HIGHER ORDER FUNCTIONS
-- function currying: "carregar" parâmetros usando partial application; 
-- partial application: injetar parcial e progressivamente parâmetros de uma função com mais de um argumento, gerando novas funções super especializadas;
-- partially apply infix functions with SECTIONS (surround with parentheses)
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

-- map, filter
quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
  let smallerSorted = quicksort (filter (<=x) xs)  
      biggerSorted = quicksort (filter (>x) xs)   
  in  smallerSorted ++ [x] ++ biggerSorted

sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

-- Collatz sequences
-- We take a natural number. If that number is even, we divide it by two. If it's odd, we multiply it by 3 and then add 1 to that
chain :: (Integral a) => a -> a
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

listOfFuns = map (*) [0..]
(listOfFuns !! 4) 5

-- Lambda functions (anonymous)
numLongChains :: Int  
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- foldr, foldl (reduce)
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs 

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
-- use right folds when we're building up new lists from a list

-- foldl1, foldr1 use the first element as starting point

-- scanr, scanr1, scanl, scanl1: analogus to folds, but returns a list with all accumulator states

-- low precedence (right-associative) function application ($): avoid parentheses
sum $ map sqrt [1..130]
sum $ filter (> 10) $ map (*2) [2..10]

-- function composition (create new functions on-the-fly
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- point free / pointless style: operate only by using curried functions
-- from: sum' xs = foldl (+) 0 xs
-- to:   sum'    = foldl (+) 0
-- don't overuse function composition in complex/long functions, use let or split into sub-functions instead
oddSquareSum :: Integer  
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum :: Integer
oddSquareSum =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimt

-- MODULES
-- each module contains functions and types
-- Prelude module (default)
-- import <module name>
import Data.List
import Data.List (nub, sort)
import Data.List hiding (nub)
import qualified Data.Map
import qualified Data.Map as M

---------------
-- Data.List --
---------------

-- intersperse
intersperse '.' "MONKEY"    -- "M.O.N.K.E.Y"
intersperse 0 [1,2,3,4,5,6] -- [1,0,2,0,3,0,4,0,5,0,6]

-- intercalate
intercalate " " ["hey","there","guys"]        -- "hey there guys"
intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]] -- [1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]

-- transpose
transpose [[1,2,3],[4,5,6],[7,8,9]] -- [[1,4,7],[2,5,8],[3,6,9]]
transpose ["hey","there","guys"]    -- ["htg","ehu","yey","rs","e"]

-- map
-- filter
-- concat
-- foldl, foldr, foldl1, foldr1 (lazy)
-- foldl', foldr', foldl1', foldr1' (strict)
concat ["foo","bar","car"]       -- "foobarcar"
concat [[3,4,5],[2,3,4],[2,1,1]] -- [3,4,5,2,3,4,2,1,1]

-- concatMap (map + concat)
-- and
and $ map (>4) [5,6,7,8]    -- True
and $ map (==4) [4,4,4,3,4] -- False

-- or
or $ map (==4) [2,3,4,5,6,1] -- True
or $ map (>4) [1,2,3]        -- False

-- any
any (==4) [2,3,5,6,1,4]                  -- True
any (`elem` ['A'..'Z']) "HEYGUYSwhatsup" -- True

-- all
all (>4) [6,9,10]                        -- True
all (`elem` ['A'..'Z']) "HEYGUYSwhatsup" -- False

-- iterate
take 10 $ iterate (*2) 1             -- [1,2,4,8,16,32,64,128,256,512]
take 3  $ iterate (++ "haha") "haha" -- ["haha","hahahaha","hahahahahaha"]

-- splitAt
splitAt 3 "heyman"                       -- ("hey","man")
splitAt 100 "heyman"                     -- ("heyman","")
splitAt (-3) "heyman"                    -- ("","heyman")
let (a,b) = splitAt 3 "foobar" in b ++ a -- "barfoo"

-- takeWhile
takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1] -- [6,5,4]
takeWhile (/=' ') "This is a sentence"       -- "This"

-- dropWhile
dropWhile (/=' ') "This is a sentence" -- " is a sentence"
dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1] -- [3,4,5,4,3,2,1]

-- span
let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest -- "First word: This, the rest: is a sentence"

-- break (negated span)
break (==4) [1,2,3,4,5,6,7] -- ([1,2,3],[4,5,6,7])

-- sort
sort [8,5,3,2,1,6,4,2]          -- [1,2,2,3,4,5,6,8]
sort "This will be sorted soon" -- "    Tbdeehiillnooorssstw"

-- group
group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7] -- [[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]

-- inits
inits "w00t" -- ["","w","w0","w00","w00t"]

-- tails
tails "w00t" -- ["w00t","00t","0t","t",""]

-- isInfixOf
"cat"  `isInfixOf` "im a cat burglar" -- True
"Cat"  `isInfixOf` "im a cat burglar" -- False
"cats" `isInfixOf` "im a cat burglar" -- False

-- isPrefixOf (beginning)
"hey" `isPrefixOf` "hey there!"    -- True
"hey" `isPrefixOf` "oh hey there!" -- False

-- isSuffixOf (end)
"there!" `isSuffixOf` "oh hey there!" -- True
"there!" `isSuffixOf` "oh hey there"  -- False

-- elem: check if an element is inside a list

-- notElem: check if an element isn't inside a list

-- partition
partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy" -- ("BOBMORGAN","sidneyeddy")  
partition (>3) [1,3,5,6,3,2,1,0,3,7]                -- ([5,6,7],[1,3,3,2,1,0,3])

-- find
find (>4) [1,2,3,4,5,6] -- Just 5  
find (>9) [1,2,3,4,5,6] -- Nothing  

-- elemIndex
:t elemIndex  
4 `elemIndex` [1,2,3,4,5,6]  -- Just 3
10 `elemIndex` [1,2,3,4,5,6] -- Nothing

-- elemIndices
' ' `elemIndices` "Where are the spaces?" -- [5,9,13]

-- findIndex: like find, but returns the index
findIndex (==4) [5,3,2,1,6,4] -- Just 5  
findIndex (==7) [5,3,2,1,6,4] -- Nothing

-- findIndices
findIndices (`elem` ['A'..'Z']) "Where Are The Caps?" -- [0,6,10,14]

-- zip3, zip4, zip5, zip6, zip7: zip more than 2 lists

-- zipWith3, zipWith4, zipWith5, zipWith6, zipWith7: zipWith more than 2 lists

-- lines
lines "first line\nsecond line\nthird line" -- ["first line","second line","third line"]

-- unlines
unlines ["first line", "second line", "third line"] -- "first line\nsecond line\nthird line\n"

-- words
words "hey these are the words in this sentence"               -- ["hey","these","are","the","words","in","this","sentence"]
words "hey these           are    the words in this\nsentence" -- ["hey","these","are","the","words","in","this","sentence"]

-- unwords
unwords ["hey","there","mate"] -- "hey there mate"

-- nub
nub [1,2,3,4,3,2,1,2,3,4,3,2,1] -- [1,2,3,4]
nub "Lots of words and stuff"   -- "Lots fwrdanu" 

-- delete
delete 'h' "hey there ghang!"                             -- "ey there ghang!"  
delete 'h' . delete 'h' $ "hey there ghang!"              -- "ey tere ghang!"
delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!" -- "ey tere gang!"

-- \\
[1..10]         \\ [2,5,9] -- [1,3,4,6,7,8,10]  
"Im a big baby" \\ "big"   -- "Im a  baby" 

-- union
"hey man" `union` "man what's up" -- "hey manwt'sup"  
[1..7]    `union` [5..10]         -- [1,2,3,4,5,6,7,8,9,10]

-- intersect
[1..7] `intersect` [5..10] -- [5,6,7]

-- insert
insert 4 [3,5,1,2,8,2] -- [3,4,5,1,2,8,2]  
insert 4 [1,3,4,4,1]   -- [1,3,4,4,4,1]

-- genericLength, genericTake, genericDrop, genericSplitAt, genericIndex, genericReplicate: Num / Integral typeclasses version

-- nubBy, deleteBy, unionBy, intersectBy, groupBy: customizable equality by passing a function

-- sortBy, insertBy, maximumBy, minimumBy: customizable ordering by passing a function

---------------
-- Data.Char --
---------------

-- isControl: checks whether a character is a control character.
-- isSpace: checks whether a character is a white-space characters. That includes spaces, tab characters, newlines, etc.
-- isLower: checks whether a character is lower-cased.
-- isUpper: checks whether a character is upper-cased.
-- isAlpha: checks whether a character is a letter.
-- isAlphaNum: checks whether a character is a letter or a number.
-- isPrint: checks whether a character is printable. Control characters, for instance, are not printable.
-- isDigit: checks whether a character is a digit.
-- isOctDigit: checks whether a character is an octal digit.
-- isHexDigit: checks whether a character is a hex digit.
-- isLetter: checks whether a character is a letter.
-- isMark: checks for Unicode mark characters. Those are characters that combine with preceding letters to form latters with accents. Use this if you are French.
-- isNumber: checks whether a character is numeric.
-- isPunctuation: checks whether a character is punctuation.
-- isSymbol: checks whether a character is a fancy mathematical or currency symbol.
-- isSeparator: checks for Unicode spaces and separators.
-- isAscii: checks whether a character falls into the first 128 characters of the Unicode character set.
-- isLatin1: checks whether a character falls into the first 256 characters of Unicode.
-- isAsciiUpper: checks whether a character is ASCII and upper-case.
-- isAsciiLower:checks whether a character is ASCII and lower-case.

-- generalCategory
generalCategory ' '             -- Space
generalCategory 'A'             -- UppercaseLetter
generalCategory 'a'             -- LowercaseLetter
generalCategory '.'             -- OtherPunctuation
generalCategory '9'             -- DecimalNumber
map generalCategory " \t\nA9?|" -- [Space,Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]

-- toUpper: converts a character to upper-case. Spaces, numbers, and the like remain unchanged.
-- toLower: converts a character to lower-case.
-- toTitle: converts a character to title-case. For most characters, title-case is the same as upper-case.
-- digitToInt: converts a character to an Int. To succeed, the character must be in the ranges '0'..'9', 'a'..'f' or 'A'..'F'.
-- intToDigit is the inverse function of digitToInt. It takes an Int in the range of 0..15 and converts it to a lower-case character.

-- ord
ord 'a' -- 97  

--chr
chr 97 -- 'a'

--------------
-- Data.Map --
--------------

-- key-value pair data structure

-- fromList
Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")] -- fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]  
Map.fromList [(1,2),(3,4),(3,2),(5,5)]                                           -- fromList [(1,2),(3,2),(5,5)]

-- empty
Map.empty -- fromList []

-- insert
Map.insert 3 100 Map.empty -- fromList [(3,100)]

-- null
Map.null Map.empty                    -- True
Map.null $ Map.fromList [(2,3),(5,5)] -- False

-- size
Map.size Map.empty                                      -- 0  
Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)] -- 5  

-- singleton
Map.singleton 3 9                  -- fromList [(3,9)]  
Map.insert 5 9 $ Map.singleton 3 9 -- fromList [(3,9),(5,9)]  

-- lookup: works like the Data.List lookup, only it operates on maps. It returns Just something if it finds something for the key and Nothing if it doesn't.

-- member
Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)] -- True  
Map.member 3 $ Map.fromList [(2,5),(4,5)]       -- False 

-- map & filter: just like in Lists, but operate only on map values, not keys

-- toList: inverse or fromList

-- keys: returns the list of keys of a map

-- elems: returns the list of values of a map

-- fromListWith: like fromList, but if a duplicate key is found, the function we pass is used to combine the values of those keys into some other value.
Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)] -- fromList [(2,100),(3,29),(4,22)]

-- insertWith: like fromListWith, but for insert operations.

--------------
-- Data.Set --
--------------

-- naturally ordered, unique elements
-- faster than list nub to remove duplicates of big lists (but nub requires only Eq typeclass elements, Sets also require Ord)

-- fromList
-- intersection
-- difference
-- union
-- null
-- size
-- member
-- empty
-- singleton
-- insert
-- delete
-- map
-- filter

----------------------
-- Creating Modules --
----------------------

-- Sphere.hs
module Geometry.Sphere
( sphereVolume  
, sphereArea
) where  
  
sphereVolume :: Float -> Float  
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
  
sphereArea :: Float -> Float  
sphereArea radius = 4 * pi * (radius ^ 2) 

---------------------------
-- Making new Data types --
---------------------------

-- data <type> = <value constructor> | <value constructor> | ...
data Bool = False | True
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647

module Shapes   
( Point(..)
, Shape(..)
, surface
) where 

data Point = Float Float deiriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
map (Circle 10 20) [4,5,6,6] -- [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]

-- Record syntax: automatically creates field lookup functions, also prints types differently
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

let Me = { firstname = "Felipe", lastname = "Monobe", age = 26, height = 1.70, phoneNumber = "99316-8064", flavor = "vanilla" }
lastName Me -- "Monobe"
Me { firstname = "Felipe", lastname = "Monobe", age = 26, height = 1.70, phoneNumber = "99316-8064", flavor = "vanilla" }
