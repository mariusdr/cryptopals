module Main where 
import HexParser (decodeHex, encodeHex)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Bits as BIT
import Data.Word (Word8)
import Data.Char (chr, ord)
import Data.Map (Map, unionsWith, singleton, (!), member)
import Data.List (head)

xor :: ByteString -> ByteString -> ByteString 
xor xs ys = BS.pack $ map (uncurry BIT.xor) $ BS.zip xs ys

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
repeatN n x = BS.pack $  replicate n x

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

main :: IO () 
main = mapM_ (putStrLn .(\(c,i,bs) -> showSolution c i bs)) solutions
    where 
        inp = decodeHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
        solutions = solver inp



