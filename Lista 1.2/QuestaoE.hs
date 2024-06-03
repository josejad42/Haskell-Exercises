data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

faces :: Direction -> [Command] -> Direction
faces d [] = d
faces d (x:xs) = faces (getDir (rot (getInt d) x)) xs

getInt :: Direction -> Int
getInt North = 0
getInt East = 1
getInt South = 2
getInt West = 3

rot :: Int -> Command -> Int
rot x TurnLeft = x - 1
rot x TurnRight = x + 1
rot x c = x

getDir :: Int -> Direction
getDir x | mod x 4 == 0 = North
         | mod x 4 == 1 = East
         | mod x 4 == 2 = South
         | otherwise = West

main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result