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

ex3 :: Document
ex3 = 
  [ Paragraph "Remember that multi plne with no separation are grouped together to a single paragraph"
  , OrderedList 
    [
    "Item 1 of a list"
    , "Item 2 of a sampe list"
    ]
  ]

ex4 :: Document
ex4 = 
  [ Heading 1 ""
  , Paragraph ""
  , Paragraph ""
  , CodeBlock 
    [ "main = putStrLn \"Hello, Haskell!\""
    ]
  , CodeBlock
    [ "➜ ghc hello.hs"
    , "[1 of 1] Compiling Main             ( hello.hs, hello.o )"
    , "Linking hello ..."
    ]
  , Paragraph "GHC created the following files:"
  , UnorderedList
    [ "hello.hi - Haskell interface file"
    , "hello.o - Object file, the output of the compiler before linking"
    , "hello (or hello.exe on Microsoft Windows) - A native runnable executable."
    ]
  , Paragraph "GHC will produce an executable when the source file satisfies both conditions:"
  , OrderedList
    [ "Defines the main function in the source file"
    , "Defines the module name to be Main, or does not have a module declaration"
    ]
  , Paragraph "Otherwise, it will only produce the .o and .hi files."
  ]

main :: IO()
main = do 
  print $ ex1
  print $ ex2
  print $ ex3
  print $ ex4
