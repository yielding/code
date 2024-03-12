main :: IO()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_ 
    "My title"
    ( append_
      ( h1_ "Heading")
      ( append_
        ( p_ "Prargraph #1")
        ( p_ "Prargraph #2")
      )
    )

newtype Html 
  = Html String

newtype Structure 
  = Structure String

type Title
  = String

html_ :: Title -> Structure -> Html
html_ title content =
  Html 
    (el "html" 
      (el "head" (el "title" title)
        <> el "body" (getStructureString content)
      )
    )

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) =
  Structure (a <> b)

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

render :: Html -> String
render html =
  case html of
    Html str -> str