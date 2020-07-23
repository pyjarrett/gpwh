data Box a = Box a deriving Show

instance Functor Box where
    fmap func (Box a) = Box (func a)

morePresents :: Int -> Box a -> Box [a]
morePresents n b = replicate n <$> b

myBox :: Box Int
myBox = Box 1

unwrap :: Box a -> a
unwrap (Box b) = b

wrappedBox :: Box (Box Int)
wrappedBox = fmap Box myBox

unwrappedBoxInBox = unwrap wrappedBox