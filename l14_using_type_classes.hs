data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord, Enum)

instance Show SixSidedDie where
    show S1 = "1"
    show S2 = "2"
    show S3 = "3"
    show S4 = "4"
    show S5 = "5"
    show S6 = "6"


data Name = Name (String, String) deriving (Eq)

instance Show Name where
    show (Name (first, last)) = first ++ " " ++ last

instance Ord Name where
    compare (Name (first1, last1)) (Name (first2, last2)) = compare (last1, first1) (last2, first2)

names :: [Name]
names = [
    Name ("Joe", "Smith"),
    Name ("Art", "Arvy"),
    Name ("Jane", "Blondie"),
    Name ("Jack","Smith"),
    Name ("Andy", "Griffith")]

