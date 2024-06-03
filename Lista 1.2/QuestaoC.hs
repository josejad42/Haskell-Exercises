uncommonFromTwoSentences :: String -> String -> [String]
uncommonFromTwoSentences s1 s2 =  bSort (getStrings (words (convertMin s2)) (words (convertMin s1)) [] ++ 
                                         getStrings (words (convertMin s1)) (words (convertMin s2)) [] )

getStrings :: [String] -> [String] -> [String] -> [String]
getStrings [] s l = []
getStrings (x:xs) s l | not (elem x s) && not (elem x xs) && not (elem x l) = x : (getStrings xs s (x:l))
                      | otherwise = getStrings xs s (x:l)

bSort :: [String] -> [String]
bSort [] = []
bSort (x:xs) = bSort (filter (<x) xs) ++ [x] ++ bSort (filter (>= x) xs)

convertMin :: String -> String
convertMin [] = []
convertMin (x:xs) | x == ' ' = ' ' : convertMin xs
                  | (fromEnum x) < 97 = (toEnum ((fromEnum x) + 32) :: Char) : convertMin xs
                  | otherwise = x : convertMin xs

main = do
    sentence_1 <- getLine
    sentence_2 <- getLine
    let result = uncommonFromTwoSentences sentence_1 sentence_2
    print result