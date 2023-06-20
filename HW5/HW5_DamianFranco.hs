-- Damian Franco
-- CS-558
-- Homework 5

-- Problem 5.1 (a)
data Term = Var String
          | Abs String Term
          | App Term Term
          deriving Show

testData1 = Abs "a" (Abs "b" (Var "a"))
testData2 = App (Abs "z" (App (Var "z") (Var "z"))) (Abs "q" (Var "q"))
testData3 = Abs "x" (Abs "y" (Var "y"))
testData4 = Abs "d" (Var "d")

-- Problem 5.1 (b)
var2String :: Term -> String
var2String (Var x) = x

subst :: Term -> String -> Term -> Term
subst (Var s) x t = if x == s
                    then t
                    else Var s
subst (Abs t1 t2) x t = if x == t1 
                        then (Abs (var2String t) (subst t2 x t)) 
                        else (Abs t1 (subst t2 x t))
subst (App t1 t2) x t = App (subst t2 x t) (subst t1 x t)

substTest1 = subst testData1 "a" (Var "x")
substTest2 = subst testData2 "z" (Var "a")
substTest3 = subst testData3 "y" (Var "w")

-- Problem 5.1 (c)
isValue :: Term -> Bool
isValue (Var s) = False
isValue (Abs t1 t2) = True
isValue (App t1 t2) = False

isValueTest1 = isValue testData1
isValueTest2 = isValue testData2
isValueTest3 = isValue testData3
isValueTest4 = isValue testData4

-- Problem 5.1 (d)
eval1 :: Term -> Maybe Term
eval1 (Var s) = Nothing -- Stuck at Variable
eval1 (Abs t1 t2) = Nothing -- Found a Value
eval1 (App (Abs t1 t2) s) | isValue s = Just (subst t2 t1 s) -- E-AbsApp
eval1 (App s t2) | isValue s = let Just evalT = eval1 t2 -- E-App1
                               in Just (App s evalT)
eval1 (App t1 s) | isValue s = eval1 t1 -- Found a value
eval1 (App t1 t2) = let Just evalT = eval1 t1 -- E-App2
                    in Just (App evalT t2)

testData5 = App (Abs "x" (Var "x")) (Abs "z" (App (Abs "x" (Var "x")) (Var "z")))

eval1Test1 = eval1 testData1
eval1Test2 = eval1 testData2
eval1Test3 = eval1 testData3
eval1Test4 = eval1 testData4
eval1Test5 = eval1 testData5

-- Problem 5.1 (e)
eval :: Term -> [Maybe Term]
eval (Var s) = []
eval (Abs t1 t2) = []
eval (App t1 t2) = [eval1 (App t1 t2)] ++ eval t1 ++ eval t2

testData6 = App (Abs "x" (Var "x")) (App (Abs "x" (Var "x")) (Abs "z" (App (Abs "x" (Var "x")) (Var "z"))))

evalTest1 = eval testData5
evalTest2 = eval testData6

-- Probem 5.1 (f)
testDatai = Abs "a" (Abs "b" (Var "a"))
testDataii = App (Abs "z" (App (Var "z") (Var "z"))) (Abs "q" (Var "q"))
testDataiii = Abs "x" (Abs "y" (Var "y"))

-- Problem 5.1 (g)
testData7 = App (App (Abs "a" (Abs "b" (Var "a"))) (App (App (Abs "z" (Var "z")) (Var "z")) (Abs "q" (Var "q")))) (Abs "x" (Abs "y" (Var "y")))
evalTest3 = eval testData7
