echo :: IO ()
echo = getStrLn >>= putStrLn

main :: IO ()
main = echo
