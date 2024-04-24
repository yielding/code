newtype Html = Html String
newtype Structure = Structure String

escape :: String -> String
escape =
  let
    escapeChar c =
      case c of
        '<'  -> "&lt;"
        '>'  -> "&gt;"
        '&'  -> "&amp;"
        '"'  -> "&quot;"
        '\'' -> "&#39;"
        _    -> [c] --  Non-exhaustive patterns in case
  in
    concat . map escapeChar

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape -- newtype constructors can be used as function!

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

main :: IO()
main = print $ escape "<hi>"
