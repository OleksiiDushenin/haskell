import Data.Char (isDigit)
import Data.Char (digitToInt)

adder :: IO ()
adder = do
            x <- readCount
            print x

readCount :: IO Int
readCount = do
                putStrLn "How many numbers? "
                count <- getChar
                putChar '\n'
                if isDigit count
                then return (digitToInt count)
                else do
                        putStrLn "Invalid number"
                        readCount

writeLine :: String -> IO ()
writeLine [] = return ()
writeLine (x:xs) = do
                        putChar x
                        writeLine xs

writeLineNew :: String -> IO ()
writeLineNew x = do
                        writeLine x
                        putChar '\n'

readLine :: IO String
readLine = do
                x <- getChar
                if x == '\n'
                then return []
                else
                    do
                        xs <- readLine
                        return (x : xs)

main = adder
