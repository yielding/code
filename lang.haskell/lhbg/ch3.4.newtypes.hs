newtype Html = Html String
newtype Structure = Structure String

getStructureString0 :: Structure -> String
getStructureString0 (Structure str) = str

getStructureString1 :: Structure -> String
getStructureString1 struct =
  case struct of
    Structure str -> str

p_ :: String -> Structure
p_ = Structure . el "p"

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

render :: Html -> String
render html =
  case html of 
    Html str -> str


s = Structure "hello"

main :: IO()
main = do 
  print $ getStructureString0 s
  print $ getStructureString1 s
