module MaybeMonad where

import Prelude hiding((<$>), (<*>), (>>=))

-- Mapping Maybe Values --
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing  = Nothing
mapMaybe f (Just x) = Just $ f x

addOne :: Num a => Maybe a -> Maybe a
addOne = mapMaybe (1+)

square :: Num a => Maybe a -> Maybe a
square = mapMaybe (^2)

maybeLength :: Maybe [a] -> Maybe Int
maybeLength = mapMaybe length

maybeShow :: Show a => Maybe a -> Maybe String
maybeShow = mapMaybe show

-- Applying Maybe Functions --
pureMaybe :: a -> Maybe a
pureMaybe = Just

-- apply는 '함수'를 적용하다는 의미
applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe (Just g) (Just x) = Just $ g x
applyMaybe _        _        = Nothing

-- match associativity / precedence of the lib-defined operators
infixl 4 <$>
infixl 4 <*>

(<$>) = mapMaybe
(<*>) = applyMaybe

lift2Maybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
lift2Maybe f ma mb
  -- pureMaybe f `applyMaybe` ma `applyMaybe` mb
  -- f `mapMaybe` ma `applyMaybe` mb
  = f <$> ma <*> mb

lift3Maybe :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
lift3Maybe f ma mb mc
  -- pureMaybe f `applyMaybe` ma `applyMaybe` mb `applyMaybe` mc
  -- f `mapMaybe` ma `applyMaybe` mb `applyMaybe` mc
  = f <$> ma <*> mb <*> mc

lift4Maybe :: (a -> b -> c -> d -> e) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e
lift4Maybe f ma mb mc md
  = f <$> ma <*> mb <*> mc <*> md

-- Sequencing Maybe Actions

foo :: Maybe (a -> Maybe b) -> Maybe a -> Maybe b
foo mf ma = applyMaybeFancy id $ applyMaybe mf ma

applyMaybeFancy :: (a -> Maybe b) -> Maybe a -> Maybe b
applyMaybeFancy _ Nothing   = Nothing
applyMaybeFancy  f (Just x) = f x

andThenMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
andThenMaybe = flip applyMaybeFancy

(>>=) = andThenMaybe
(<=<) :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
(f <=< g) x = g x `andThenMaybe` f

guardMaybe :: Bool -> Maybe ()
guardMaybe True  = Just ()
guardMaybe False = Nothing

-- Person
type Person = String
father :: Person -> Maybe Person
father a = Just a -- instead of undefined

grandfather :: Person -> Maybe Person
grandfather = father <=< father

sibling :: Person -> Person -> Maybe ()
sibling x y =
  (guardMaybe $ x /= y) >>= \() ->
  father x              >>= \fx ->
  father y              >>= \fy ->
  guardMaybe $ fx == fy

{-
grandfather :: Person -> Maybe Person
grandfather p =
  case father p of
    Nothing -> Nothing
    Just fp -> father fp
-}

{-
grandfather :: Person -> Maybe Person
grandfather p =
  father `applyMaybeFancy` (father `applyMaybeFancy` (Just p))
  father p `andThenMaybe` (\fp -> father fp `andThenMaybe` (\ffp -> Just ffp))
  father p `andThenMaybe` (\fp -> father fp `andThenMaybe` pureMaybe)
  -- father p `andThenMaybe` father
-}

{-
sibling :: Person -> Person -> Maybe Bool
sibling x y =
  x /= y && sameParent where
    sameParent =
      case (father x, father y) of
        (Just fx, Just fy) -> fx == fy && x /= y
        _                  -> False

sibling x y =
  father x `andThenMaybe` (\fx ->
    father y `andThenMaybe` (\fy ->
      guardMaybe $ fx == fy && x /= y
  ))
-}

-- main
main :: IO ()
main = do
  print $ square (Just 2)
  print $ maybeShow $ square (Just 2)
  print $ grandfather "b" -- Just  (Just b) == Just b

