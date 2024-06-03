data Tree t = Node t (Tree t) (Tree t) | Nilt
    deriving (Read, Show)

dna1 :: Tree Int -> [String]
dna1 Nilt = []
dna1 x = reverseAll(format(convertBase(convertMod((showMyTree x)))) 8 "" [])

showMyTree :: Tree Int -> [Int] -- in order traversal --
showMyTree Nilt = []
showMyTree (Node t t1 t2) = (showMyTree t1) ++ [t] ++ (showMyTree t2)

convertMod :: [Int] -> [Int]
convertMod [] = []
convertMod (x:xs) = mod x 5 : convertMod xs

convertBase :: [Int] -> [Char]
convertBase [] = []
convertBase (0:xs) = 'E' : convertBase xs
convertBase (1:xs) = 'M' : convertBase xs
convertBase (2:xs) = 'A' : convertBase xs
convertBase (3:xs) = 'C' : convertBase xs
convertBase (4:xs) = 'S' : convertBase xs

format :: [Char] -> Int -> String -> [String] -> [String]
format [] d s l = s : l 
format (c:cs) d s l| d > 0 = format cs (d-1) (c:s) l
                   | otherwise = format (c:cs) 8 "" (s:l)

reverseAll :: [String] -> [String]
reverseAll [] = []
reverseAll (x:xs) = (reverseAll xs) ++ [reverse x]

main :: IO ()
main = do

  input <- getLine

  let result = dna1 (read input :: Tree Int)

  print result