myRemove test [] = []
myRemove test (x:xs) = if test x
    then myRemove test xs
    else x:myRemove test xs

myProduct list = foldl (*) 1 list

myRCons xs y = y:xs
myReverse xs = foldl myRCons [] xs

myFoldl binaryFn initial [] = initial
myFoldl binaryFn initial (x:xs) = myFoldl binaryFn newInit xs
    where newInit = binaryFn initial x

myFoldr binaryFn initial [] = initial
myFoldr binaryFn initial (x:xs) = binaryFn x rightResult
    where rightResult = myFoldr binaryFn initial xs