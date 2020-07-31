primesToN :: Integer -> [Integer]
primesToN n = filter isComposite twoThroughN
    where twoThroughN = [2 .. n]
          composite = pure (*) <*> twoThroughN <*> twoThroughN
          isComposite = not . (`elem` composite)