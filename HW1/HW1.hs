-- Problem 1.1 (a)
atLeastOneTrue :: [Bool] -> Bool
atLeastOneTrue [] = False
atLeastOneTrue (x:xs) = if x == True 
                        then True 
                        else atLeastOneTrue xs
-- Tests:
-- atLeastOneTrue [False, False, False]
-- atLeastOneTrue [True, True, True]
-- atLeastOneTrue [9 == 10, 4 == 5, 6 == 7]
-- atLeastOneTrue [9 == 9, 4 == 5, 6 == 7]
-- atLeastOneTrue [9 < 1, 1 < 0, 70 < 20, 80 < 40, 40 < 20, 90 < 2]
-- atLeastOneTrue [9 < 1, 1 < 0, 70 < 20, 80 < 40, 40 < 20, 90 > 2]

-- Problem 1.1 (b)
onlyLastOneTrue :: [Bool] -> Bool
onlyLastOneTrue (x:xs) = if xs == []
                         then (if x == True 
                               then True 
                               else False)
                         else (if x == True 
                               then False 
                               else onlyLastOneTrue xs)
-- Tests:
-- onlyLastOneTrue [True, True, False]
-- onlyLastOneTrue [True, False, True]
-- onlyLastOneTrue [False, False, True]
-- onlyLastOneTrue [9 == 10, 2 == 2, 7 == 7]
-- onlyLastOneTrue [9 == 10, 2 == 3, 7 == 7]
-- onlyLastOneTrue [4 > 3, 7 < 2, 10 < 9]
-- onlyLastOneTrue [4 < 3, 7 < 2, 10 < 9]
-- onlyLastOneTrue [4 < 3, 7 < 2, 10 > 9]

-- Problem 1.1 (c)
switch :: [Int] -> [Int]
switch [] = []
switch (x:xs) = if xs == []
                then [x]
                else(head xs) : x : switch (tail xs)
-- Tests:
-- switch [1,2,3,4,5]
-- switch [1,3,5,7,9,11,13,15]
-- switch [0,2,4,6,8,10,12]
-- switch [10,20]

-- Problem 1.2 (a)
divisibleBy :: Int -> [Int]
divisibleBy n = [x | x <- [1,2..], x `mod` n == 0]
-- Tests:
-- take 10 (divisibleBy 6)
-- take 20 (divisibleBy 6)
-- take 2 (divisibleBy 64)
-- take 8 (divisibleBy 64)
-- take 20 (divisibleBy 128)

-- Problem 1.2 (b)
-- take 20 (divisibleBy 9)
-- drop 10 (take 18 (divisibleBy 9))