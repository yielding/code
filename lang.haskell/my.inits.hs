-- NOTICE 아래 함수 하나씩 이해하기
-- map reverse . scanl (flip (:)) []
-- haskell은 List.init과, List.inits가 다르다.
-- 이런 어휘들이 전쟁을 이기는 무기가 되면 좋겠다.
--
--                 (:) ::  a -> [a] -> [a]
--            flip (:) :: [a] -> a -> [a]
-- scanl (flip (:)) [] :: [a] -> [[a]]
myInits0 :: [a] -> [[a]]
myInits0 = scanl (flip (:)) []

myInits1 :: [a] -> [[a]]
myInits1 xs = scanl (flip (:)) [] xs
-- givin xs == [1, 2, 3]
-- [[]]
-- [[], [1]] ( [1] == 1:[])
-- [[], [1], [2, 1]] ([2, 1] == 2:[1])
-- [[], [1], [2, 1], [3, 2, 1]] ([3, 2, 1] == 3:[2, 1])

myInits2 :: [a] -> [[a]]
myInits2 xs = map reverse . scanl (flip (:)) [] $ xs

myInits3 :: [a] -> [[a]]
myInits3 xs = (map reverse . scanl (flip (:)) []) xs

myInits4 :: [a] -> [[a]]
myInits4 = map reverse . scanl (flip (:)) []
-- == (map reverse) . scanl (flip (:)) []
-- 즉, myInits0 함수의 적용 결과를 reverse_map

main :: IO()
main = do
  print $ flip (:) [1, 2] 3          -- 3:[1, 2] == [3, 1, 2] : 인자를바꾸어 적용
  print $ scanl (/) 64 [2, 4, 8]     -- [64, 64/2, 64/2/4, 64/2/4/8] 
                                     -- 첫 인자를 결과에 담고, 
                                     -- 두 번째 인자의 각 요소를 순차적으로 주어진 함수에 적용
  print $ scanl (flip (:)) [] [1..3] -- [[],[1],[2,1],[3,2,1]]
                                     -- [[]]
                                     -- [[], 1:[]] -> [[], [1]]
                                     -- [[], [1], 2:[1]] -> [[], [1], [2, 1]]
                                     -- [[], [1], [2, 1], 3:[2, 1]] -> [[], [1], [2, 1], [3, 2, 1]]
  print $ myInits1 [1..3]            -- [[],[1],[2,1],[3,2,1]]
  print $ myInits2 [1..3]            -- [[],[1],[1,2],[1,2,3]]
  print $ myInits3 [1..3]            -- [[],[1],[1,2],[1,2,3]]
  print $ myInits4 [1..3]            -- [[],[1],[1,2],[1,2,3]]

  print $ (sqrt . abs . negate) 2

