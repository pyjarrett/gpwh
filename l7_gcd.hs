myGCD a b = if remainder == 0 then b else myGCD b remainder
    where remainder = a `mod` b

myRecursiveGCD a 0 = a
myRecursiveGCD a b = myRecursiveGCD b (a `mod` b)