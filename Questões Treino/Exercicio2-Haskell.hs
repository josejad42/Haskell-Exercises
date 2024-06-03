-- Questao 1 --

addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos 1 = " "
addEspacos x = " " ++ (addEspacos (x-1))

-- Questao 2 --

paraDireita :: Int -> String -> String
paraDireita x s = addEspacos(x) ++ s

-- Questao 3 --

vendas :: Int -> Int 
vendas x = x*x

totalVendas :: Int -> Int
totalVendas 0 = vendas 0
totalVendas x = vendas x + totalVendas(x-1)

mediaVendas :: Int -> Float
mediaVendas x =  ((fromIntegral(totalVendas x)::Float) / (fromIntegral(x+1)::Float))

cabecalho :: String
cabecalho = "Semana Vendas\n"

imprimeSemanas :: Int -> String
imprimeSemanas 0 = (show 0) 
                   ++ (paraDireita 6 (show (vendas 0))) 
                   ++ "\n"

imprimeSemanas x = imprimeSemanas (x-1) 
                   ++ (show x) 
                   ++ (paraDireita 6 (show(vendas x))) 
                   ++ "\n"

imprimeVendas :: Int -> IO()
imprimeVendas x = putStrLn(cabecalho 
                           ++ imprimeSemanas x
                           ++ ( "Total: " ++ show(totalVendas x) 
                           ++ "\n")
                           ++ ("Media: " ++ show(mediaVendas x)))