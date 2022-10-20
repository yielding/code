import System.IO
import System.Process (system)

cls  :: IO ()
cls = do system("clear")
         return ()

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr $ "\ESC[" ++ show x ++ ";" ++ show y ++ "H"

writeat :: Pos -> String -> IO ()
writeat p xs = 
  do goto p
     putStr xs
                  
seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = 
  do a
     seqn as

-- Game of Life

width :: Int
width = 5

height :: Int
height = 5

type Board = [Pos]

glider    :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

showcells :: Board -> IO ()
showcells board = seqn[writeat p "0" | p <- board]

isAlive   :: Board -> Pos -> Bool
isAlive board pos = elem pos board

isEmpty   :: Board -> Pos -> Bool
isEmpty board pos = not (isAlive board pos)

neighbors :: Pos -> [Pos]
neighbors (x, y) = map wrap[
  (x - 1, y - 1), (x, y - 1), (x + 1, y - 1), 
  (x - 1, y + 0),             (x + 1, y + 0), 
  (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width ) + 1, 
               ((y - 1) `mod` height) + 1)

liveneighbors :: Board -> Pos -> Int 
liveneighbors b = length . filter (isAlive b) . neighbors

survivors :: Board -> [Pos]
survivors board = 
  [p | p <- board, elem (liveneighbors board p) [2, 3]]

births board = 
  [p | p <- rmdups (concat (map neighbors board)),
       isEmpty board p,
       liveneighbors board p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups(filter (/= x) xs)

nextgen :: Board -> Board
nextgen board = survivors board ++ births board

life :: Board -> IO ()
life board = do cls
                showcells board
                wait 100000
                life (nextgen board)

wait :: Int -> IO()
wait n = seqn[return () | _ <- [1..n]]


main = do life glider
