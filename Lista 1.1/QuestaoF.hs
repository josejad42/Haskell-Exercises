minMaxCartao :: String -> (Double, Double)
minMaxCartao s = aux1 (99999, 0) s

aux1 :: (Double, Double) ->  String -> (Double, Double)
aux1 a (x:xs) | x == ';' = aux2 a xs
              | otherwise = aux1 a xs

aux2 :: (Double, Double) -> String -> (Double, Double)
aux2 a (x:xs) | x == ';' = aux3 (load a xs) xs
              | otherwise = aux2 a xs

aux3 :: (Double, Double) -> String -> (Double, Double)
aux3 a (x:xs) | xs == [] = a
              | x == ';' = aux1 a xs
              | otherwise = aux3 a xs

load :: (Double, Double) -> String -> (Double, Double)
load (a,b) s | ((valueDouble s) < a) && ((valueDouble s) > b) = ((valueDouble s), (valueDouble s))
             | (valueDouble s) < a = ((valueDouble s), b)
             | (valueDouble s) > b = (a, (valueDouble s))
             | otherwise = (a,b)

valueDouble :: String -> Double
valueDouble x = read (value x) :: Double

value :: String -> String
value (x:xs) | x == ';' = ""
             | otherwise = [x] ++ (value xs)

main = do
    a <- getLine
    let result = minMaxCartao a
    print result
