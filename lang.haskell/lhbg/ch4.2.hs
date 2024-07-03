import Prelude hiding (const, replicate, odd, even)
import Markup

increment n = n + 1

decrement m = m - 1

const a b = a

add :: Integer -> Integer -> Integer
add n m =
  if m /= 0
     then add (increment n) (decrement m)
     else n

replicate :: Int -> a -> [a]
replicate n x =
   if n <= 0
      then []
      else x : replicate (n - 1) x

even :: Int -> Bool
even n =
   if n == 0
      then True
      else odd (n - 1)

odd :: Int -> Bool
odd n = 
   if n == 0
      then False
      else even (n - 1)

parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
   let 
      paragraph = Paragraph (unlines (reverse currentParagraph))
   in
      case txts of
        [] -> [paragraph]
        currrentLine : rest ->
          if trim currrentLine == ""
            then
              paragraph : parseLines [] rest
            else
              parseLines (currrentLine : currentParagraph) rest

trim :: String -> String
trim = unwords . words

main = do
  if const (add 3 2) (decrement 3) == 5
     then putStrLn "Yes."
     else putStrLn "No."

  print $ lines "hello\nworld"
  print $ parse "hello\nworld"