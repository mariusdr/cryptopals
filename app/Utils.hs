module Utils where 

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.HashMap.Strict 
    (HashMap, adjust, empty, findWithDefault, foldrWithKey, fromList, singleton, unionWith, insertWith)
import Data.Hashable (Hashable)
import qualified Data.Bits as BITS

-- unionsWith of Data.Map adapted for HashMaps
unionsWith :: (Eq k, Hashable k, Foldable f) => (v -> v -> v) -> f (HashMap k v) -> HashMap k v
unionsWith f = foldl (unionWith f) empty

computeByteHistogram :: ByteString -> HashMap Word8 Int 
computeByteHistogram xs = computeByteHistogram' (BS.unpack xs) empty 
    where 
        insertFn = insertWith (+)
        computeByteHistogram' [] map = map 
        computeByteHistogram' (y:ys) map = computeByteHistogram' ys (insertFn y 1 map)


computeByteEntropy :: ByteString -> Float 
computeByteEntropy xs = sum $ map (\x -> - prob x * logProb x) [0..255]
    where 
        hist = computeByteHistogram xs

        prob :: Word8 -> Float
        prob x = fromIntegral (findWithDefault 0 x hist) / 255

        logProb :: Word8 -> Float
        logProb x = if abs (prob x) < 0.0001 then 0.0 else log (prob x)

missingBytes :: ByteString -> Int 
missingBytes xs = 256 - length (computeByteHistogram xs)

xor :: ByteString -> ByteString -> ByteString 
xor xs ys = BS.pack $ map (uncurry BITS.xor) $ BS.zip xs ys

hammingDist :: ByteString -> ByteString -> Int 
hammingDist xs ys = sum $ map BITS.popCount $ BS.unpack $ xor xs ys

transposeByteMatrix :: [[Word8]] -> [[Word8]]
transposeByteMatrix ([]:_) = []
transposeByteMatrix x = map head x : transposeByteMatrix (map tail x)