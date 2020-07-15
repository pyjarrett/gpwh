cup flOz = \message -> message flOz
getOz aCup = aCup (\flOz -> flOz)
drink aCup ozDrank = if left > 0
                     then cup left
                     else cup 0
    where left = getOz aCup - ozDrank

isEmpty aCup = getOz aCup == 0