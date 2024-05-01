module Html.Internal where

-- * Types

newtype Html 
  = Html String

newtype Structure 
  = Structure String

type Title
  = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title content =
  Html 
    (el "html" 
      (el "head" (el "title" (escape title))
        <> el "body" (getStructureString content)
      )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

-- what a really compact and precise function!
-- the last one [structure] is omitted
ul_ :: [Structure] -> Structure
ul_ =
  Structure . concat . map (el "li" . getStructureString)

-- concat :: [[a]] -> [a]

ol_ :: [Structure] -> Structure
ol_ =
  Structure . el "ol" . concat . map (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

--append_ :: Structure -> Structure -> Structure
--append_ (Structure a) (Structure b) =
--  Structure (a <> b)

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)

-- * Render

-- 단지 Html을 벋겨내는데 이렇게 좋은 함수 이름을 쓴다는 말이야?
render :: Html -> String
render html =
  case html of
    Html str -> str

-- * Utilities

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

escape :: String -> String
escape =
  let 
    escapeChar c = 
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
  in
    concat . map escapeChar