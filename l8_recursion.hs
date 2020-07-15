myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs) = x:remaining
    where remaining = myTake (n-1) xs

myCycle (x:xs) = x:myCycle (xs ++ [x])

-- Q1
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Q2
fastFib a b 1 = a
fastFib a b counter = fastFib b (a+b) (counter-1)

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)