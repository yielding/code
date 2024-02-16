data Suit = Spade | Club | Diamond | Heart 
  deriving (Eq, Ord, Enum)

instance Show Suit where
  show Spade   = "S" -- "♠"
  show Club    = "C" -- "♣"
  show Diamond = "D" -- "♦"
  show Heart   = "H" -- "♥"

data Rank 
  = Two   | Three | Four | Five | Six   | Seven 
  | Eight | Nine  | Ten  | Jack | Queen | King 
  | Ace
  deriving (Eq, Ord, Enum, Bounded)

instance Show Rank where
  -- one-liner ?
  show a = case lookup a (zip [Two .. Ace] [2 .. 14]) of
             Just n  -> show n
             Nothing -> show ""
  -- show Two   = "2"
  -- show Three = "3"
  -- show Four  = "4"
  -- show Five  = "5"
  -- show Six   = "6"
  -- show Seven = "7"
  -- show Eight = "8"
  -- show Nine  = "9"
  -- show Ten   = "10"
  -- show Jack  = "11"
  -- show Queen = "12"
  -- show King  = "13"
  -- show Ace   = "1"

data Card = Card Suit Rank
  deriving (Eq, Ord)

instance Show Card where
  -- 아래의 (Card a b)가 정말 어려웠다. a, b
  show (Card a b) = show a ++ show b

main :: IO()
main = do
  print $ Spade < Heart
  print $ [Spade .. Heart]
  print $ show (lookup Two (zip [Two .. Ace] [2 .. 14]))
  print $ succ Spade
  print $ Card Spade Ace
  print $ Card <$> [Spade ..] <*> [Two ..]