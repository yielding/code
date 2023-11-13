import Control.Monad

data Sheep 
  = Sheep { name   :: String
          , mother :: Maybe Sheep
          , father :: Maybe Sheep
          }

instance Show Sheep where
  show s = show (name s)


parent :: Sheep -> Maybe Sheep
parent s = father s `mplus` mother s

maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s 
--  = do m <- mother s
--       father m
  = return s  >>= \ms -> 
    mother ms >>= \m  -> 
    father m

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s
--  = do f  <- father s
--       gm <- mother f
--       mother gm
  = return s  >>= \ms -> 
    father ms >>= \f  ->
    mother f  >>= \gm ->
    mother gm

mothersPaternalGrandfater :: Sheep -> Maybe Sheep
mothersPaternalGrandfater s
--  = do m <- mother s
--       gf <- father m
--       father gf
  = return s  >>= \ms ->
    mother ms >>= \m  ->
    father m  >>= \gf ->
    father gf

-- thid builds our sheep family tree
breedSheep :: Sheep
breedSheep 
  = let adam   = Sheep "Adam"   Nothing Nothing
        eve    = Sheep "Eve"    Nothing Nothing
        uranus = Sheep "Uranus" Nothing Nothing
        gaea   = Sheep "Gaea"   Nothing Nothing
        kronos = Sheep "Gronos" (Just gaea)  (Just uranus)
        holly  = Sheep "Holly"  (Just eve)   (Just adam)
        roger  = Sheep "Roger"  (Just eve)   (Just kronos)
        molly  = Sheep "Molly"  (Just holly) (Just roger)
    in Sheep "Dolly" (Just molly) Nothing

main :: IO ()
main 
  = let dolly = breedSheep
    in  do print (maternalGrandfather dolly)
           print (parent dolly)