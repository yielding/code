-- Revised Version by Evan Martin (original version by Vincent Burns)
import Data.Time

bailout :: Double
bailout = 16

maxIterations :: Int
maxIterations = 1000

mandelbrot :: Double -> Double -> Int
mandelbrot x y = mb 0 0 0 where
  cr = y - 0.5
  ci = x
  mb i zr zi | i > maxIterations = 0
             | otherwise = 
                 if zi2 + zr2 > bailout then i else
                 mb (i+1) (zr2 - zi2 + cr) (t + t + ci)
                 where t   = zr*zi
                       zr2 = zr*zr
                       zi2 = zi*zi

renderMandelbrot :: IO ()
renderMandelbrot =
  mapM_ putStrLn $ map mbLine bounds
  where mbLine y = [mbChar x y | x <- bounds]
        mbChar x y = if mandelbrot (x / 40) (y / 40) == 0 then '*' else ' '
        bounds = [-39..39]

main :: IO ()
main = do
  putStrLn "Rendering..."
  start <- getCurrentTime
  renderMandelbrot
  end <- getCurrentTime
  let diff = diffUTCTime end start
  print (diff)
