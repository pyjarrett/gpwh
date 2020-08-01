months :: [String]
months = [
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"
    ]

days :: [Int]
days = [
    31,
    28,
    31,
    30,
    31,
    30,
    31, -- July
    31, -- August
    30, 
    31,
    30,
    31
    ]

allDays = [ m ++ " " ++ (show z) | (m,y) <- (zip months days), z <- [1 .. y]]


allDaysDo = do
    (m, y) <- (zip months days)
    z <- [1 .. y]
    return (m ++ " " ++ (show z))

allDaysMonad =
    nameDayPairs >>=
        (\(monthName, numDays) -> 
            [1 .. numDays] >>= (\day -> return(monthName ++ " " ++ (show day)))
        )
    where nameDayPairs = zip months days
          