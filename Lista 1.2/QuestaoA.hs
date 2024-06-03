score :: Float -> [Int]-> [Int]
score x [a,b,c] | x >= 0.1 && x <= 0.3 = [a+1,b,c]
                | x >= 0.4 && x <= 0.7 = [a, b+1, c] 
                | x >= 0.8 = [a, b, c+1]
                | otherwise = [a,b,c]

dotProduct :: String -> String -> Float
dotProduct [] y = 0
dotProduct x [] = 0 
dotProduct (x:xs) (y:ys) | x == y = 1 + (dotProduct xs ys)
                         |otherwise = dotProduct xs ys

dna2 :: [String] -> [String] -> [Int]
dna2 [] y = [0,0,0]
dna2 x [] = [0,0,0]
dna2 (x:xs) (y:ys) = score ((dotProduct x y)/ toEnum (max (length x) (length y)) :: Float ) (dna2 xs ys)

main = do
  firstExtract <- words <$> getLine                
  secondExtract <- words <$> getLine
  let result = dna2 firstExtract secondExtract
  print result