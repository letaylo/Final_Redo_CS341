module Test1 (remove, threeN, prefix, sublist, removeAll, singleton, breakList, minusPairs) where

remove :: (Eq a) => [a] -> a -> [a]
remove [] y = []
remove (x:xs) y
 | x == y = xs
 | otherwise = x : (remove xs y)
 
threeN :: [Int] -> [Int]
threeN [] = []
threeN (n:ns)
 | n `elem` ns    = []
 | n `mod` 2 == 0 = threeN ((n `div` 2):n:ns)
 | otherwise      = threeN ((3 * n - 1):n:ns)
 
prefix :: (Eq a) => [a] -> [a] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys)
 | x == y    = prefix xs ys
 | otherwise = False

sublist :: (Eq a) => [a] -> [a] -> Bool
sublist _ [] = False
sublist xs (y:ys)
 | prefix xs (y:ys) == False = sublist xs ys
 | otherwise                 = True 

removeAll :: (Eq a) => [a] -> a -> [a]
removeAll xs y = [x | x <- xs, x /= y]

singleton :: [a] -> [[a]]
singleton xs = [[x] | x <- xs]

breakList :: [(a,[b])] -> [(a,b)]
breakList x = [(one,two) | (one,twos) <- x, two <- twos]

minusPairs :: (Num a, Ord a) => [(a,a)] -> [a]
minusPairs x = [ (two - one) | (one,two) <- x, two > one ]