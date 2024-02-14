--
-- haskell은 List.init과, List.inits가 다르다.
-- 이런 어휘들이 전쟁을 이기는 무기가 되면 좋겠다.
--
--                 (:) ::  a -> [a] -> [a]
--            flip (:) :: [a] -> a -> [a]
-- scanl (flip (:)) [] :: [a] -> [[a]]
myInits0 :: [a] -> [[a]]
myInits0 = scanl (flip (:)) []

myInits1 :: [a] -> [[a]]
myInits1 = map reverse . scanl (flip (:)) []

main :: IO ()
main = do
  print $ myInits0 [1..5]
  print $ myInits1 [1..5]