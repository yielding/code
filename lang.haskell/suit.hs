data Suit = Spade | Club | Diamond | Heart 
  deriving (Eq, Ord, Enum)

instance Show Suit where
  show Spade   = "♠"
  show Club    = "♣"
  show Diamond = "♦"
  show Heart   = "♥"

main :: IO()
main = do
  print $ Spade < Heart
  print $ [Spade .. Heart]
  print $ succ Spade