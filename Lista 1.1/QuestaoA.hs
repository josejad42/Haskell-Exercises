logMes :: String -> String -> Double
logMes a s = foldl (+) 0 (sumValue a (search1 s []) [])

search1 :: String -> [(String, Double)] -> [(String, Double)]
search1 (x:xs) a | xs == [] = a
                 | x == ' ' = search1 xs ((getMes xs, read(getValue xs) :: Double):a)
                 | otherwise = search1 xs a

getValue :: String -> String
getValue (x:xs) |x == ';' = getValueAux1 xs
                |otherwise = getValue xs

getValueAux1 :: String -> String
getValueAux1  (x:xs) |x == ';' = getValueAux2 xs
                     |otherwise = getValueAux1 xs

getValueAux2 :: String -> String
getValueAux2 (x:xs) | xs == [] = []
                    | x == ';' = []
                    | otherwise = [x] ++ getValueAux2 xs

getMes :: String -> String
getMes (x:xs) | x == ';' = []
              | otherwise = [x] ++ getMes xs

sumValue :: String -> [(String, Double)] -> [Double] -> [Double]
sumValue a (l:lx) ld | lx == [] = verify a l ld
                     | otherwise = sumValue a lx (verify a l ld)

verify :: String -> (String, Double) -> [Double] -> [Double]
verify a (b,c) ld | a == b = c:ld
                 | otherwise = ld

main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result
