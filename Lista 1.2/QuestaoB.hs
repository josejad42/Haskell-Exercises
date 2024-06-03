palavrasFrequentes :: [String] -> [String]
palavrasFrequentes s = freq (qSort (qSort2(aux s []))) [] 3

freq :: [(String, Int)] -> [String] -> Int -> [String]
freq [] ls p = []
freq x ls 0 = []
freq ((s, n):xs) ls p | not (elem s ls) = s: freq xs (s:ls) (p-1)
                      | otherwise = freq xs ls p

aux :: [String] -> [(String, Int)] -> [(String, Int)]
aux [] l = []
aux (x:xs) l = (x , numberElement (x:xs) x) : aux xs l

numberElement :: [String] -> String -> Int
numberElement [] s = 0
numberElement (x:xs) s | x == s = 1 + numberElement xs s
                    | otherwise = numberElement xs s

myFilter :: (Int -> Bool) -> [(String, Int)] -> [(String, Int)]
myFilter f [] = []
myFilter f ((s, n):xs) | f n = (s,n) : myFilter f xs
                       | otherwise = myFilter f xs


myFilter2 :: (Int -> Bool) -> [(String, Int)] -> [(String, Int)]
myFilter2 f [] = []
myFilter2 f ((s, n):xs) | f (length s) = (s,n) : myFilter2 f xs
                       | otherwise = myFilter2 f xs

qSort :: [(String, Int)] -> [(String, Int)]
qSort [] = []
qSort ((s, n):xs) = qSort (myFilter (>n) xs) ++ [(s, n)] ++ qSort (myFilter (<= n) xs)

qSort2 :: [(String, Int)] -> [(String, Int)]
qSort2 [] = []
qSort2 ((s, n):xs) = qSort2 (myFilter2 (< length s) xs) ++ [(s, n)] ++ qSort2 (myFilter2 (>= length s) xs)


main = do
       lista <- getLine
       print $ palavrasFrequentes (read lista :: [String])