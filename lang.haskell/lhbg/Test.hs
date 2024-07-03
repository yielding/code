data Color
  = RGB Word Word Word

getBluePart :: Color -> Word
getBluePart color =
  case color of
    RGB _ _ blue -> blue

data Brigntness
  = Dark
  | Bright

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | WHite

data AnsiColor
  = AnsiColor Brigntness EightColor

ansiColorToBGA :: AnsiColor -> Color
ansiColorToBGA ansiColor =
  case ansiColor of
    AnsiColor Dark Black ->
      RGB 0 0 0
    AnsiColor Bright Black ->
      RGB 85 85 85
    AnsiColor Bright Red ->
      RGB 255 85 85

isBright :: AnsiColor -> Bool
isBright color =
  case color of
    AnsiColor Bright _ -> True
    AnsiColor Dark _ -> False

isEmpty :: [a] -> Bool
isEmpty list =
  case list of
    [] -> True
    _ : _ -> False

safeHead :: [a] -> Maybe a
safeHead list =
  case list of
    [] -> Nothing
    x : _ -> Just x

exactlyTwo :: [a] -> Maybe (a, a)
exactlyTwo list =
  case list of 
    [x, y] -> Just (x, y)
    _ -> Nothing

main :: IO()
main = do 
  print $ safeHead [1..10]
  print $ exactlyTwo [2, 3]
