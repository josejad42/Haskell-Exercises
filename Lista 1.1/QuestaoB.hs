maquinaSomarAux :: [Int] -> [Int]
maquinaSomarAux (x:xs)| (x == 0) && (xs == []) = []
                      | (x == 0) && ((head xs) == 0) = []
                      | x == 0 = maquinaSomar xs
                      | (not (haltVerify (x:xs))) = []
                      | (haltVerify (x:xs)) = (sumZero (x:xs)) : maquinaSomar (next (x:xs))

maquinaSomar :: [Int] -> [Int]
maquinaSomar [] = []
maquinaSomar x = maquinaSomarAux 

next :: [Int] -> [Int]
next (x:xs) | x == 0 = xs
            | otherwise = next xs

sumZero :: [Int] -> Int
sumZero (x:xs) | x == 0 = 0
               | otherwise = x + sumZero xs

haltVerify :: [Int] -> Bool
haltVerify (x:xs) | x == 0 = True
                  | xs == [] = False
                  | otherwise = (haltVerify xs)

main = do
       lista <- getLine
       print $ maquinaSomar (read lista :: [Int])