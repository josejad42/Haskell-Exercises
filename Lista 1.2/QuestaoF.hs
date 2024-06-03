data Tree t = Node t (Tree t) (Tree t) 
              | Nilt
              deriving (Read)

alturaArvore :: Tree t -> Int
alturaArvore Nilt = 0
alturaArvore (Node t Nilt Nilt) = 1
alturaArvore (Node t t1 t2) = 1 + (max (alturaArvore t1) (alturaArvore t2))

main = do
       a <- getLine
       let result = alturaArvore (read a::Tree Int)
       print result