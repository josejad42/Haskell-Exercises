htodAux :: Char -> Int
htodAux '0' = 0
htodAux '1' = 1
htodAux '2' = 2
htodAux '3' = 3
htodAux '4' = 4
htodAux '5' = 5
htodAux '6' = 6
htodAux '7' = 7
htodAux '8' = 8
htodAux '9' = 9
htodAux 'A' = 10
htodAux 'B' = 11
htodAux 'C' = 12 
htodAux 'D' = 13
htodAux 'E' = 14
htodAux 'F' = 15

fator :: String -> Int
fator ax = 16 ^ (length ax)

htod :: String -> In
htod "" = 0
htod (x:ax) = (htodAux x) * (fator ax) + htod ax 

htobAux :: Int -> String
htobAux 1 = "1"
htobAux x = show(mod x 2) ++ htobAux (div x 2)

reversal :: String -> String
reversal "" = "

numZeros :: Int -> Int
numZeros x | x<=4 = 4
           | otherwise = 4 + numZeros(x-4)
        
zerar :: (Int,Int) -> String
zerar (x,y) | y == numZeros x = ""
            | otherwise = ("0" ++ zerar (x,y+1))

completaZeros :: String -> String
completaZeros ax = zerar (length ax, length ax ) ++ ax

htob :: String -> String
htob x = completaZeros(reversal(htobAux (htod x)))

main = do
    s <- getLine
    let result = htob s
    print result