harmonic n = foldr (+) 0 elements
    where elements = map (1 /) [1..n]

bookHarmonic n = sum (take n seriesValues)
    where seriesPairs = zip (cycle [1]) [1 ..]
          seriesValues = map (\pair -> (fst pair) / (snd pair)) seriesPairs