import Data.Map (Map)

sublist :: [Int] -> [[Int]]
sublist [] = [[]]
sublist (y:ys) = map (y:) (sublist ys)++sublist ys

replic :: [Int] -> [[Int]]
replic x = addindex 1 x

repeatNTimes 1 a = [[a]]
repeatNTimes n x = map (x:) (repeatNTimes (n - 1) x)

addindex n [] = []
addindex n (x:xs) = repeatNTimes n x ++ addindex (n + 1) xs

myfun f
	|mod f 2 ==0 = f-1
	|otherwise =f+1

change []=[]

change x = map myfun x

split [] n
	|n>0 = error " split greater than the length of the list"

split xs 0 =([],xs)

split (x:xs) (n) 
	|n<0 =error " split negative"
	|n > 0  = (x: (fst (split xs (n-1))),snd(split xs (n-1)))


