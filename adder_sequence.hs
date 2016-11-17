import Data.Char (isDigit)
import Data.Char (digitToInt)

adder :: IO ()
adder = do
            x <- readInteger "How many numbers? "
            ns <- sequence (adder_add x [])
            putStrLn ("Result: " ++ show (sum ns))

adder_add :: Int -> [IO Int] -> [IO Int]
adder_add 0 m = m
adder_add n m = adder_add (n - 1) (m ++ [z])
                    where z = readInteger "Enter number: "

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
