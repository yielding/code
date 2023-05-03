import Data.Char

type Bit = Int

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin1 . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int1) . chop8

chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

bin2int1 :: [Bit] -> Int
bin2int1 bits = sum [w*b | (w, b) <- zip weights bits]
  where
    weights = iterate (*2) 1

bin2int2 bits = foldr (\x y -> x + 2*y) 0 bits

int2bin1 :: Int -> [Bit]
int2bin1 0 = []
int2bin1 n = n `mod` 2 : int2bin1(n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

main = do
  let bins = encode "abc"

  print $ transmit "abc"

  print $ decode bins
  print $ chop8 $ bins
  print $ bins
  print $ bin2int2 [0, 1, 1]
  print $ int2bin1 13
  print $ make8 [1, 2, 3]