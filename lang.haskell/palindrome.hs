isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome s = s == reverse s

main :: IO()
main = do
  print $ isPalindrome "aba"
  print $ isPalindrome [1, 2, 1]
