myElem e xs = length matching /= 0
    where equality = (==) e
          matching = filter equality xs
