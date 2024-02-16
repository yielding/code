-- 아래 Student 타입은 다르게 표현하면
-- Student :: Integer -> String -> Int -> Student
-- 
-- 즉, [정수, 문자열, 정수] 3개의 인자를 받아 Student를 만들어내는 함수
-- 마찬가지로
--
-- Student Integer
-- Student Integer :: String -> Int -> Student
--
-- Student Integer String
-- Student Integer String :: Int -> Student
--
-- Student Integer String Int
-- Student Integer String Int :: -> Student

data Student 
  = Student { id :: Integer,  name :: String, mark :: Int }
    deriving(Show)

names :: [(Integer, String)]
names = [(1, "yielding"), (2, "kamin"), (3, "gunhee")]

marks :: [(Integer, Int)]
marks = [(1, 100), (2, 200), (3, 300)]

lookupStudent :: Integer -> Maybe Student
--lookupStudent sid = pure (Student sid) <*> lookup sid names <*> lookup sid marks
lookupStudent sid = Student sid <$> lookup sid names <*> lookup sid marks

main :: IO()
main = do
  print $ lookup 1 names
  print $ lookup 1 marks
  print $ lookupStudent 1
