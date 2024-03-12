import Html

main :: IO()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_ 
    "My title"
    (append_
      (h1_ "Heading")
      (append_
        (p_ "Prargraph #1")
        (p_ "Prargraph #2")
      )
    )