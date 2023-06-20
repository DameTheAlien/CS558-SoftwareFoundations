-- Damian Franco
-- CS-558
-- Homework 2


-- Problem 2.1 (a)
foldri :: (Int -> a -> b -> b) -> b -> [a] -> b
foldri f v [] = v
foldri f v (x:xs) = helper 1 f v (x:xs)
                    where helper :: Int -> (Int -> a -> b -> b) -> b -> [a] -> b
                          helper acc f v [] = v
                          helper acc f v (x:xs) = f acc x (helper (acc + 1) f v xs)
-- Tests:
-- foldri (\idx -> \x -> \acc -> (x + 1) : acc) [] [0,1]
-- foldri (\idx -> \x -> \acc -> (x * 2) : acc) [] [1,2,3,4,5]
-- foldri (\idx -> \x -> \acc -> (x - 2) : acc) [] [4,8,16,32,64,128]
-- foldri (\idx -> \x -> \acc -> x : acc) [] [6,-1,2,-4]


-- Problem 2.1 (b)
expand :: [Int] -> [Int]
expand [] = []
expand (x:xs) = foldri (\idx -> \x -> \acc -> (replicate idx x) ++ acc) [] (x:xs)
-- Tests:
-- expand [0,1]
-- expand [1,2,3,4,5]
-- expand [4,8,15,16]
-- expand [7,10,13,18,20,24,28]