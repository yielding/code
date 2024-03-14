import Markup

example1 :: Document
example1 =
  [ Heading 1 "Welcome"
  , Paragraph "Hello, World!"
  ]

example2 :: Document
example2 = 
  [ Paragraph "Remember that multiple lines with no separation are grouped together to a single paragraph"
  , OrderedList 
    [ "Item 1 of a list"
    , "Item 2 of the same list"
    ]
  ]

main :: IO()
main = do
  print $ example1
  print $ example2