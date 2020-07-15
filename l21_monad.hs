import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

inputMap = Map.fromList [(1, "Paul")]

maybeMain :: Maybe String
maybeMain = do
    name <- Map.lookup 1 inputMap
    let statement = helloPerson name
    return name
