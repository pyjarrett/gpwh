import qualified Data.Map as Map

data RobotPart = RobotPart {
      name :: String
    , description:: String
    , cost :: Double
    , count :: Int
    } deriving(Show)

leftArm :: RobotPart
leftArm = RobotPart {
    name = "Left arm"
    , description = "left arm for face punching!"
    , cost = 1000.0
    , count = 3
    }

rightArm :: RobotPart
rightArm = RobotPart {
    name = "Right arm"
    , description = "right arm for waving"
    , cost = 1024.00
    , count = 5
    }

robotHead :: RobotPart
robotHead = RobotPart {
    name = "robot head"
    , description = "This head looks mad"
    , cost = 5092.25
    , count = 2
    }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat [ "<h2>", partName, "</h2>"
    , "<p><h3>desc</h3>", partDesc
    , "</p><p><h3>cost</h3>", partCost
    , "</p><p><h3>count</h3>", partCount, "</p>"]
    where partName = name part
          partDesc = description part
          partCost = show (cost part)
          partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where keys = [1,2,3]
          vals = [leftArm, rightArm, robotHead]
          keyVals = zip keys vals

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = snd <$> (Map.toList partsDB)

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

printCost :: Maybe Double -> IO ()
printCost (Just val) = print val
printCost Nothing = putStrLn "Value not found."

main :: IO ()
main = do
    line <- getLine
    let id = read line :: Int
    let maybePart = Map.lookup id partsDB
    printCost (cost <$> maybePart)
    --print line