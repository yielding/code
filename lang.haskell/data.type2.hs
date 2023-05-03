data Suit = Club | Diamond | Heart | Spade
  deriving (Read, Show, Enum, Eq, Ord)

data CardValue = Two | Three | Four
  | Five | Six | Seven | Eight | Nine | Ten
  | Jack | Queen | King | Ace 
  deriving (Read, Show, Enum, Eq, Ord)

data Card = Card { value :: CardValue, suit :: Suit }
  deriving (Read, Show, Eq)

instance Ord Card where
  compare c1 c2 = compare (value c1, suit c1) (value c2, suit c2)
    
instance Enum Card where
  toEnum   n = let (v, s) = n `divMod` 4 in Card (toEnum v) (toEnum s)
  fromEnum c = fromEnum (value c) * 4 + fromEnum (suit c) 

type Deck = [Card]

deck::Deck
deck = [Card val su | val <- [Two .. Ace], su <- [Club .. Spade]]

main = do 
  print(deck)