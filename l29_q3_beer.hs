bought = [6, 12]
drank = [4]
friends = [2, 3]
beerPerPerson = [3, 4]

-- Does the roommate count?
numPeople = (+2) <$> friends
totalBeersNeeded = (*) <$> numPeople <*> beerPerPerson
onHand = (-) <$> bought <*> drank

beerToBuy = (-) <$> totalBeersNeeded <*> onHand