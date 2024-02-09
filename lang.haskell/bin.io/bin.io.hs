module Main where

import Data.Word
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BStrict

-- Convert one bytestring chunk to the list of integers
-- and append the result of conversion of the later chunks.
-- It actually appends only closure which will evaluate next
-- block of numbers on demand.
toNumbers :: BStrict.ByteString -> [Word32] -> [Word32]
toNumbers chunk rest = chunkNumbers ++ rest
    where
    getNumberList = replicateM (BStrict.length chunk `div` 4) getWord32le
    chunkNumbers = runGet getNumberList (BS.fromStrict chunk)

main :: IO()
main = do
    -- every operation below is done lazily, consuming input as necessary
    input <- BS.readFile "in.dat"
    let inNumbers = BS.foldrChunks toNumbers [] input
    let outNumbers = map (\x -> if x < 1000 then 2*x else x) inNumbers
    let output = runPut (mapM_ putWord32le outNumbers)
    -- There lazy bytestring output is evaluated and saved chunk
    -- by chunk, pulling data from input file, decoding, processing
    -- and encoding it back one chunk at a time
    BS.writeFile "out.dat" output