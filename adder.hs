import Data.Char (isDigit)
import Data.Char (digitToInt)

adder :: IO ()
adder = do
            x <- readInteger "How many numbers? "
            result <- adder_add x 0
            putStrLn ("Result: " ++ show result)

adder_add :: Int -> Int -> IO Int
adder_add 0 m = do
                    return m
adder_add n m = do
                    x <- readInteger "Enter number: "
                    adder_add (n - 1) (x + m)

readInteger :: String -> IO Int
readInteger m =  do
                    putStrLn m
                    line <- getLine
                    if isInt line
                        then do
                            let value = read line :: Int
                            return value
                        else do
                            value <- (readInteger m)
                            return value

isInt :: String -> Bool
isInt xs = and [isDigit x | x <- xs]

main = adder
