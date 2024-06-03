suaviza :: [Float] -> [Float]
suaviza [] = []
suaviza (x:[]) = [x]
suaviza (x:xs) = x : applyMedia x xs

media :: Float -> Float -> Float -> Float
media a b c = (a + b + c) / 3

applyMedia :: Float -> [Float] -> [Float]
applyMedia a (x:[]) = [x]
applyMedia a (x:xs) = (media a x (head(xs))) : applyMedia x xs

main = do
        lista <- getLine
        print $ suaviza (read lista :: [Float])