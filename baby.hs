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
    in  sum belowLimit
