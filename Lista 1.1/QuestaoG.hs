mul2 :: [Int] -> [Int] -> [Int]
mul2 [] [] = []
mul2 [] (b:bx) = [0] ++ (mul2 [] bx)
mul2 (a:ax) [] = (mul2 ax []) ++ [0]
mul2 (a:ax) (b:bx) = [a * b] ++ (mul2 ax bx)

main = do
    sa <- getLine
    let a = read sa :: [Int]
    sb <- getLine
    let b = read sb :: [Int]
    let result = mul2 a b
    print result