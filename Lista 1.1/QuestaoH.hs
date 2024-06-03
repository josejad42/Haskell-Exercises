addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos 1 = " "
addEspacos x = " " ++ (addEspacos (x-1))

paraDireita :: Int -> String -> String
paraDireita x s = addEspacos(x) ++ s

parseInput str = let [n, s] = words str
                 in (read n, s)
main :: IO()
main = interact $ uncurry paraDireita.parseInput