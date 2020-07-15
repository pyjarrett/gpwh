simple = (\x -> x)

-- makeChange owed given = (\change -> 
--                             if change > 0
--                             then change
--                             else 0) (owed - given)

makeChange = (\owed given -> 
                if (owed - given) > 0
                then (owed - given)
                else 0)

sumSquareOrSquareSum x y = (\sumSquare squareSum -> if sumSquare > squareSum
    then sumSquare
    else squareSum) (x^2 + y^2) ((x + y)^2)
