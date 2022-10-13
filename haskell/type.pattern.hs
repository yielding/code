data Foo = Foo { x :: Integer, str :: String}

instance Eq Foo where
  (Foo x1 str1) == (Foo x2 str2) = (x1 == x2) && (str1 == str2)

data Foo2 = Foo2 { xx :: Integer, ss :: String }
  deriving (Eq, Ord, Show)

-- 리스트 보충설명에서 map을 처음 소개할 때, 리스트 원소에 대한 아주 
-- 구체적인 함수에서 시작하여 일반화를 거쳐 map을 모든 종류의 리스트에 
-- 들어맞는 함수와 결합했다.
-- 더 일반화하여, 리스트용 map, 트리용 map 등을 만드는 대신 매핑 가능한 
-- 모든 종류의 타입에 대한 범용 map을 두는 것이 어떨까?
--

-- 리스트에 작용하는 대부분의 함수는 패턴 매칭에 허용되지 않는다. 
-- 그럼 어떤 함수들이 허용되는 걸까?
-- 한 마디로 말하면 생성자(constructor), 즉 대수 자료형(algebraic data type)의 값을 
-- 구축하는 데 쓰이는 함수들만이 가능하다. 임시로 만든 다음 예시를 보자.

main = do print "hi"