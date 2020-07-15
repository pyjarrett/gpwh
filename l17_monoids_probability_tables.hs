data Events = Events [String]
data Probabilities = Probabilities [Double]

data ProbabilityTable = ProbabilityTable Events Probabilities

createProbabilityTable (Events e) (Probabilities p) = ProbabilityTable (Events e) (Probabilities normalizedProbabilities)
    where normalizedProbabilities = map (\x -> x / totalProbabilities) p
          totalProbabilities = sum p

showPair :: String -> Double -> String
showPair event probability = mconcat [event, "|", show probability, "\n"]

instance Show ProbabilityTable where
    show (ProbabilityTable (Events events) (Probabilities probabilities)) = mconcat pairs
        where pairs = zipWith showPair events probabilities

-- Generations a combination of event event from the first list with every
-- event from the second list.
cartesianCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianCombine fn list1 list2 = zipWith fn newList1 cycleList2
    where nToAdd = length list2
          repeatedList1 = map (take nToAdd . repeat) list1
          newList1 = mconcat repeatedList1
          cycleList2 = cycle list2

instance Semigroup Events where
    (<>) (Events e1) (Events []) = Events e1
    (<>) (Events []) (Events e1) = Events e1
    (<>) (Events e1) (Events e2) = Events (cartesianCombine (\x y -> mconcat [x, "-", y]) e1 e2)

instance Monoid Events where
    mempty = Events []
    mappend = (<>)

instance Semigroup Probabilities where
    (<>) (Probabilities p1) (Probabilities []) = Probabilities p1
    (<>) (Probabilities []) (Probabilities p1) = Probabilities p1
    (<>) (Probabilities p1) (Probabilities p2) = Probabilities (cartesianCombine (*) p1 p2)

instance Monoid Probabilities where
    mempty = Probabilities []
    mappend = (<>)

instance Semigroup ProbabilityTable where
    (<>) ptable (ProbabilityTable (Events []) (Probabilities [])) = ptable
    (<>) (ProbabilityTable (Events []) (Probabilities [])) ptable = ptable
    (<>) (ProbabilityTable (Events e1) (Probabilities p1))
         (ProbabilityTable (Events e2) (Probabilities p2)) = ProbabilityTable (Events ec) (Probabilities pc)
        where ec = e1 <> e2
              pc = p1 <> p2

instance Monoid ProbabilityTable where
    mempty = ProbabilityTable (Events []) (Probabilities [])
    mappend = (<>)

--
-- DATA
--

coinEvents = Events (["heads", "tails"])
coinProbabilities = Probabilities [0.5, 0.5]
coinFlipTable = createProbabilityTable coinEvents coinProbabilities

diceEvents = Events (map show [1 .. 6])
unfairDiceProbabilities = Probabilities [1, 10, 100, 1000, 10000, 100000]
unfairDiceTable = createProbabilityTable diceEvents unfairDiceProbabilities
