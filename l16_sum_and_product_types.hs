type FirstName = String
type LastName = String
type MiddleName = String
data Name = Name FirstName LastName
    | NameWithMiddle FirstName MiddleName LastName
    | FirstNameWithTwoInits FirstName Char Char

data Creator = AuthorCreator Author | ArtistCreator Artist
data Author = Author Name
data Artist = Person Name | Band String

data Book = Book  {
      author :: Author
    , isbn :: String
    , bookTitle :: String
    , bookYear :: Int
    , bookPrice :: Double
}

data VinylRecord = VinylRecord {
      artist :: Artist
    , recordTitle :: String
    , recordYear :: Int
    , recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
      name :: String
    , description :: String
    , toyPrice :: Double
}

data Pamphlet = Pamphlet {
      title :: String
    , pamphletDescription :: String
    , contact :: Name
}

data StoreItem = BookItem Book
    | RecordItem VinylRecord
    | ToyItem CollectibleToy
    | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem pamphlet) = 0.0
