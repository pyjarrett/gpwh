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