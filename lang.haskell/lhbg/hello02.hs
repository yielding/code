main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_ 
    "My title"                   -- String
    ( append_                    -- Structure String
      ( h1_ "Heading")
      ( append_
        ( p_ "Prargraph #1")
        ( p_ "Prargraph #2")
      )
    )

--
-- String을 주면 Html을 반환해주는 함수로. 
-- Html :: String -> Html
--
newtype Html 
  = Html String

newtype Structure 
  = Structure String

type Title
  = String

html_ :: Title -> Structure -> Html
html_ title content =
  Html 
    ( el "html" 
      ( el "head" (el "title" title)
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

--
-- 주어진 Html 문서에서 string을 뽑아내는 함수
-- why? 브라우저에 출력하려면, 문자열으로의 변환이 필요. 
--
render :: Html -> String
render html =
  case html of
    Html str -> str