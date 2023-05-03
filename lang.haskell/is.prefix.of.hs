import Data.ByteString

f a b c = if isPrefixOf b a
  then a : c
  else c