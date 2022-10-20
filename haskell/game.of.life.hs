import Data.List
import Control.Monad

type Cell = (Int, Int)
type Grid = [Cell]

-- Game Logic

neighbours :: Cell -> Grid
neighbours (x, y) = do
  dx <- [-1..1]
  dy <- [-1..1]
  guard (dx /= 0 || dy /= 0)
  return (x + dx, y + dy)

step :: Grid -> Grid
step cells = do
  (newCell, n) <- frequencies $ concatMap neighbours cells
  guard $ (n == 3) || (n == 2 && newCell `elem` cells)
  return newCell

-- This is the only deviation from the Clojure version, since it is not a
-- built-in in Haskell.
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies xs = do
  x <- group $ sort xs
  return (head x, length x)


-- UI

-- Feel like I'm missing a concept. Not so happy with this function:
-- * Can `eol` be done a better way? I tried nested maps but it was urgh.
-- * `marker` seems long for a simple tenary. Same issue as `eol` I guess.
formatGrid :: Grid -> String
formatGrid grid = do
  y <- ys
  x <- xs
  [marker x y] ++ eol x
  where
    marker x y
      | (x, y) `elem` grid = '*'
      | otherwise          = ' '
    eol x
      | x == maximum xs = ['\n']
      | otherwise       = []

    xs = gridRange fst
    ys = gridRange snd
    gridRange f = [min grid .. max grid]
      where
        min = minimum . map f
        max = maximum . map f

main = do
  mapM_ printGrid . take 3 $ iterate step beacon
  where
    beacon = [(0, 0), (1, 0), (0, 1), (3, 3), (2, 3), (3, 2)]

    printGrid :: Grid -> IO ()
    printGrid grid = do
      putStrLn $ formatGrid grid
      putStrLn ""
