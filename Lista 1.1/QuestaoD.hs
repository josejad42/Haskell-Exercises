bSort :: [String] -> [String]
bSort [] = []
bSort (x:xs) = bSort (filter (<x) xs) ++ [x] ++ bSort (filter (>= x) xs)

main = do
       a <- getLine
       let result = bSort (read a :: [String])
       print result