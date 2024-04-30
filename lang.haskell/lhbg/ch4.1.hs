import Markup

ex1 :: Document
ex1 = 
  [
    Paragraph "Hello, world"
  ]

ex2 :: Document
ex2 = 
  [
    Heading 1 "Welcome",
    Paragraph "To this tut."
  ]

main :: IO()
main = print $ ex2
