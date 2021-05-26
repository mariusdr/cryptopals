module Main where 
import HexParser (decodeHex, encodeHex)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Bits as B
import Data.Word (Word8)
import Data.Char (chr, ord)
import Data.Map (Map, unionsWith, singleton, (!), member)
import Data.List (head)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.IO 
import Control.Monad


xor :: ByteString -> ByteString -> ByteString 
xor xs ys = BS.pack $ map (uncurry B.xor) $ BS.zip xs ys

hist :: String -> Map Char Int 
hist xs = unionsWith (+) $ map (flip singleton 1) xs 

score :: String -> Int 
score xs = foldr ((+) . uncurry charScore) 0 coeffs
    where
        hst = hist xs
        coeffs = zip "etaoinshrdlu" [12,11..1] ++ zip "ETAOINSHRDLU" [12,11..1] :: [(Char, Int)]
        lookup' key = if member key hst then hst ! key else 0
        charScore char coeff = lookup' char * coeff

repeatN :: Int -> Word8 -> ByteString 
repeatN n x = BS.pack $ replicate n x

solve :: ByteString -> Word8 -> ByteString 
solve xs y = xor xs ys
    where 
        ys = repeatN (BS.length xs) y

solver :: ByteString -> [(Word8, Int, ByteString)] 
solver xs = map scorer [1..255]
    where 
        decoded :: Word8 -> ByteString 
        decoded = solve xs 

        scorer :: Word8 -> (Word8, Int, ByteString)
        scorer c = (c, s, d) 
            where 
                d = decoded c
                s = score $ map (chr . fromIntegral) $ BS.unpack d

showSolution :: Word8 -> Int -> ByteString -> String 
showSolution c i bs = "Char: " ++ show c ++ " Score: " ++ show i ++ " Msg: " ++ show bs

repeatUntil :: Int -> ByteString -> ByteString 
repeatUntil = repeatUntil' BS.empty 
    where 
        repeatUntil' :: ByteString -> Int -> ByteString -> ByteString 
        repeatUntil' acc n bs | BS.length acc < n = repeatUntil' (BS.concat [acc, bs]) n bs
                              | BS.length acc >= n = BS.take n acc 

repeatedKeyXorCipher :: ByteString -> ByteString -> ByteString  
repeatedKeyXorCipher key input = xor key' input
    where
        key' = repeatUntil (BS.length input) key


hammingDist :: ByteString -> ByteString -> Int 
hammingDist xs ys = sum $ map B.popCount $ BS.unpack $ xor xs ys

main :: IO ()
main = print $ hammingDist a b 
    where 
        a = encodeUtf8 $ T.pack "this is a test"
        b = encodeUtf8 $ T.pack "wokka wokka!!!"

-- main :: IO () 
-- main = do 
--     handle <- openFile "set1challenge6input" ReadMode 
--     contents <- hGetContents handle 
--     print contents
--     hClose handle


