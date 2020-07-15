import qualified Data.Map as Map

data Box a = Box a deriving(Show)

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)

data Triple a = Triple a a a deriving(Show)

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f (Just x) = Just (f x)
maybeMap _ Nothing = Nothing

---
--- DATA
---

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)
organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7 ,13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

organTypes :: [Organ]
organTypes = [Heart .. ]

countOrgansOfType :: Organ -> Int
countOrgansOfType organ = length (filter (\(_, t) -> t == organ) (Map.toList organCatalog))

organCounts :: [(Organ, Int)]
organCounts = zip organTypes (map countOrgansOfType organTypes)

organInventory :: Map.Map Organ Int
organInventory = Map.fromList organCounts

shouldBeHeart = Map.lookup 2 organCatalog

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
    where getContents = (\id -> Map.lookup id catalog)

emptyDrawers :: Map.Map Int Organ -> Int
emptyDrawers catalog = length (filter (\x -> x == Nothing) (getDrawerContents possibleDrawers catalog))

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgans :: Organ -> [Maybe Organ] -> Int
countOrgans organ available = length (filter justOnly available)
    where justOnly = (\x -> x == Just organ)

isSomething :: Maybe Organ -> Bool
isSomething (Just _) = True
isSomething Nothing = False

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgans :: Maybe Organ -> String
showOrgans (Just organ) = show organ
showOrgans Nothing = ""

organList :: [String]
organList = map showOrgans justTheOrgans

data Container = Vat Organ | Cooler Organ | Bag Organ
instance Show Container where
    show (Vat organ) = (show organ) ++ " in a vat"
    show (Cooler organ) = (show organ) ++ " in a cooler"
    show (Bag organ) = (show organ) ++ " in a bag"

data Location = Lab | Kitchen deriving (Show)

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat organ) = (Lab, (Vat organ))
placeInLocation (Cooler organ) = (Lab, (Cooler organ))
placeInLocation (Bag organ) = (Kitchen, (Bag organ))

process :: Maybe Organ -> Maybe (Location, Container)
process (Just organ) = Just (placeInLocation (organToContainer organ))
process Nothing = Nothing

report :: Maybe (Location, Container) -> String
report (Just (location, container)) = show container ++ " in the " ++ show location
report Nothing = "container not found"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = report (process organ)
    where organ = Map.lookup id catalog


