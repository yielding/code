import Html

main :: IO()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_ "Title"
  (append_ 
    (h1_ "Head")
    (ul_ [ p_ "kamin"
         , p_ "yielding"
         , p_ "gunhee"
         ])
  )
