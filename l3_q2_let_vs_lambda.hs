
{- 
counter x = let x = x + 1
            in
              let x = x + 1
              in
                x
-}

counter x = (\x -> (\x -> x + 1) (x+1))


counter x = (\x -> x + 1
                ((\x -> x + 1)
                    ((\x -> x) x)))


