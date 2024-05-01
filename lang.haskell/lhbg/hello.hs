import Html

main :: IO()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_ "My title"
  (
    (h1_ "Heading") <> 
    (p_ "Prargraph #1") <> 
    (p_ "Prargraph #2")
  )
