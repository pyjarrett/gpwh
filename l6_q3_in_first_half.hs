inFirstHalf e list = e `elem` firstHalfList
    where firstHalfList   = take firstHalfLength list
          firstHalfLength = (length list) `div` 2
