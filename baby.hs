doubleMe :: (Num a) => a -> a
doubleMe x = x * 2

doubleUs :: Int -> Int -> Int
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: Int -> Int
doubleSmallNumber x = (if x > 100 then x else x * 2) + 1

listComprehension :: [Int] -> [Int]
listComprehension x = [x * 2 | x <- [1..10], x * 2 >= 12]

concatenateLists :: [Int] -> [Int] -> [Int]
concatenateLists x y = x ++ y

getHead :: [Int] -> Int
getHead x = head x

getTail :: [Int] -> [Int]
getTail x = tail x

getLast :: [Int] -> Int
getLast x = last x

getInit :: [Int] -> [Int]
getInit x = init x

getLength :: [Int] -> Int
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

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

getLength' :: [Int] -> Int
getLength' xs = sum [1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

getTupleFirst x = fst x
getTupleSecond x = snd x
combineTuples x y = zip x y
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

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

4 * (let a = 9 in a + 1) + 2

[let square x = x * x in (square 5, square 3, square 2)]

(let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)

(let (a,b,c) = (1,2,3) in a+b+c) * 100

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

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
