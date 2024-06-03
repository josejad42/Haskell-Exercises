type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa [] = 0
executa (x:xs) = executaAux (executaOp 0 x) xs

executaAux :: Int -> [(Comando, Valor)] -> Int
executaAux n [] = n
executaAux n (x:xs) = executaAux (executaOp n x) xs

executaOp :: Int -> (Comando, Valor) -> Int
executaOp n ("Multiplica", v) = n * v 
executaOp n ("Soma", v) = n + v 
executaOp n ("Subtrai", v) = n - v 
executaOp n ("Divide", 0) = (-666)
executaOp n ("Divide", v) = div n v

main = do
    a <- getLine
    let result = executa (read a)
    print result
