altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs

-- Long way
-- not used
altMapLong :: (a -> b) -> (a -> b) -> [a] -> [b]
altMapLong f1 f2 xs = forEven f1 f2 xs


-- not used
forEven :: (a -> b) -> (a -> b) -> [a] -> [b]
forEven _ _ [] = []
forEven f1 f2 (x:xs) = f1 x : forOdd f1 f2 xs

-- not used
forOdd :: (a -> b) -> (a -> b) -> [a] -> [b]
forOdd _ _ [] = []
forOdd f1 f2 (x:xs) = f2 x : forEven f1 f2 xs

main = print (altMap (+10) (+100) [0,1,2,3,4])

