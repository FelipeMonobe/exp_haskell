doubleMe x = x * 2
doubleUs x y = doubleMe x + doubleMe y 
doubleSmallNumber x = (if x > 100 then x else x * 2) + 1
listComprehension = [x*2 | x <- [1..10], x*2 >= 12]
concatenateLists x y = x ++ y
getHead x = head x
getTail x = tail x
getLast x = last x
getInit x = init x
getLength x = length x
isEmpty x = null x
reverseList x = reverse x
takeFirstN n x = take n x
skipFirstN n x = drop n x
getMax x = maximum x
getMin x = minimum x
sumAll x = sum x
multiplyAll x = product x
getNFromList n x = n `elem` x
makeRangeList from to = [from..to]
makeSetRangeList from to step = [from, from + step..to]
cycleList x = cycle x
repeatX x = repeat x
replicateNtimesX n x = replicate n x
filteredList = [ x | x <- [50..100], x `mod` 7 == 3]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
getTupleFirst x = fst x
getTupleSecond x = snd x
combineTuples x y = zip x y
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
