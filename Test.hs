module Test where

maximum' :: (Ord a) => [a] -> a
maximum' []     = error "max in the empy list"
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)

length' :: (Num i) => [a] -> i
length' []      = 0
length' (x:xs)  = 1 + length' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n - 1) a

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' i _
    | i <= 0    = []
take' _ []      = []
take i (x:xs)   = x : take' (i - 1) xs

zip' :: [a] -> [b] -> [(a, b)]
zip' _ []           = []
zip' [] _           = []
zip' (x:xs) (y:ys)  = (x, y):zip' xs ys

elem' :: (Eq a) => [a] -> a -> Bool
elem' [] _ = False
elem' (x:xs) a
    | a == x = True
    | otherwise = xs `elem'` a

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted   = quicksort [a | a <- xs, a <= x ]
        biggerSorted    = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

multTree :: Int -> Int -> Int -> Int
multTree x y z = x * y * z

dividenByTen :: (Floating a) => a -> a
dividenByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _             = []
zipWith' _ _ []             = []
zipWith' f (x:xs) (y:ys)    = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x
