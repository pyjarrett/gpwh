data User = User
    { name :: String
    , gamerId :: Int
    , score :: Int 
    } deriving Show

serverUserName :: Maybe String
serverUserName = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
    putStrLn "Enter a username, gamerId, and score."
    user <- User <$> getLine <*> readInt <*> readInt
    print user
