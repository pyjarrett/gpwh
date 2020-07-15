import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

file1 :: [(Int, Double)]
file1 = [ (1, 200.1), (2,  199.5), (3, 199.4)
        , (4, 198.9), (5,  199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [(Int, Double)]
file2 = [ (11, 201.6), (12, 201.5), (13, 201.5)
        , (14, 203.5), (15, 204.9), (16, 207.1)
        , (18, 210.5), (20, 208.8)]
        
file3 :: [(Int, Double)]
file3 = [ (10, 201.2), (11, 201.6), (12, 201.5)
        , (13, 201.5), (14, 203.5), (17, 210.5)
        , (24, 215.1), (25, 218.7)]

file4 :: [(Int, Double)]
file4 = [ (26, 219.8), (27, 220.5), (28, 223.8)
        , (29, 222.8), (30, 223.8), (31, 221.7)
        , (32, 222.3), (33, 220.8), (34, 219.4)
        , (35, 220.1), (36, 220.6)]
        
data TimeSeries a = TimeSeries [Int] [Maybe a]

createTimeSeries :: [Int] -> [a] -> TimeSeries a
createTimeSeries times values = TimeSeries completeTimes extendedValues
        where completeTimes = [minimum times .. maximum times]
              dataPoints = Map.fromList (zip times values)
              extendedValues = map (\t -> Map.lookup t dataPoints) completeTimes

fileToTimeSeries :: [(Int,a)] -> TimeSeries a
fileToTimeSeries timeValuePairs = createTimeSeries times values
        where (times, values) = unzip timeValuePairs

showTimeValuePair :: Show a => Int -> (Maybe a) -> String
showTimeValuePair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTimeValuePair time Nothing = mconcat [show time, "|NA\n"]

instance Show a => Show (TimeSeries a) where
        show (TimeSeries times values) = mconcat rows
                where rows = zipWith showTimeValuePair times values

ts1 :: TimeSeries Double
ts1 = fileToTimeSeries file1



ts2 :: TimeSeries Double
ts2 = fileToTimeSeries file2

ts3 :: TimeSeries Double
ts3 = fileToTimeSeries file3

ts4 :: TimeSeries Double
ts4 = fileToTimeSeries file4

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (k, Just v) = Map.insert k v myMap

combineTimeSeries :: TimeSeries a -> TimeSeries a -> TimeSeries a
combineTimeSeries (TimeSeries times1 values1) (TimeSeries [] []) = (TimeSeries times1 values1)
combineTimeSeries (TimeSeries [] []) (TimeSeries times2 values2) = (TimeSeries times2 values2)
combineTimeSeries (TimeSeries times1 values1) (TimeSeries times2 values2) = combinedSeries
        where combinedSeries = TimeSeries combinedTimes combinedValues
              combinedTimes = [minimum mergedTimes .. maximum mergedTimes]
              mergedTimes = mconcat [times1, times2]
              combinedValues = map (\t -> Map.lookup t mergedMap) combinedTimes
              mergedMap = foldl insertMaybePair map1 (zip times2 values2) 
              map1 = foldl insertMaybePair Map.empty (zip times1 values1)

instance Semigroup (TimeSeries a) where
        (<>) = combineTimeSeries

instance Monoid (TimeSeries a) where
        mempty = TimeSeries [] []
        mappend = (<>)

tsAll :: TimeSeries Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

mean :: (Real a) => [a] -> Double
mean xs =  total / count
        where total = (realToFrac . sum) xs
              count = (realToFrac . length) xs

meanTimeSeries :: (Real a) => TimeSeries a -> Maybe Double
meanTimeSeries (TimeSeries _ []) = Nothing
meanTimeSeries (TimeSeries times values) = if all (== Nothing) values
                                           then Nothing
                                           else Just avg
        where justValues = map fromJust (filter isJust values)
              avg = mean justValues

type CompareFn a = a -> a -> a
type TimeSeriesCompareFn a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTimeSeriesCompare :: Eq a => CompareFn a -> TimeSeriesCompareFn a
makeTimeSeriesCompare fn = newFn
        where newFn (_,  Nothing) (t, Just v)   = (t, Just v)
              newFn (t,  Just v)  (_, Nothing)  = (t, Just v)
              newFn (t,  Nothing) (_, Nothing)  = (t, Nothing)
              newFn (t1, Just v1) (t2, Just v2) = if fn v1 v2 == v1
                                                  then (t1, Just v1)
                                                  else (t2, Just v2)

compareTimeSeries :: Eq a => CompareFn a -> TimeSeries a -> Maybe (Int, Maybe a)
compareTimeSeries fn (TimeSeries [] []) = Nothing
compareTimeSeries fn (TimeSeries times values) = if all (== Nothing) values
                                                 then Nothing
                                                 else Just result
        where tsFn = makeTimeSeriesCompare fn
              result = foldl tsFn (0, Nothing) (zip times values)

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair _ Nothing = Nothing
diffPair Nothing _ = Nothing
diffPair (Just v1) (Just v2) = Just (v1 - v2)

diffTimeSeries :: Num a => TimeSeries a -> TimeSeries a
diffTimeSeries (TimeSeries [] []) = TimeSeries [] []
diffTimeSeries (TimeSeries times values) = TimeSeries times (Nothing:diffValues)
        where diffValues = zipWith diffPair (tail values) values

meanMaybe :: Real a => [Maybe a] -> Maybe Double
meanMaybe [] = Nothing
meanMaybe values = if any (== Nothing) values
                   then Nothing
                   else Just avg
        where avg = mean (map fromJust values)

movingAverage :: Real a => [Maybe a] -> Int -> [Maybe Double]
movingAverage [] n  = []
movingAverage values n = if length nextValues == n
                         then meanMaybe nextValues:movingAverage restValues n
                         else []
        where nextValues = take n values
              restValues = tail values

movingAverageTimeSeries :: Real a => TimeSeries a -> Int -> TimeSeries Double
movingAverageTimeSeries (TimeSeries [] []) _ = TimeSeries [] []
movingAverageTimeSeries (TimeSeries times values) n = TimeSeries times smoothedValues
        where movingAvg = movingAverage values n
              nothings = replicate (n `div` 2) Nothing
              smoothedValues = mconcat [nothings, movingAvg, nothings]