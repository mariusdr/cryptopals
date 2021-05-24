module Main where 
import HexParser (parseHexInp')
import Data.ByteString (ByteString)
import Data.Word (Word8, Word32)
import Data.Bits (shift, shiftR, (.&.))
import Data.Char (chr)
import qualified Data.ByteString as BS
import Data.IntMap.Strict (IntMap, (!), fromList)


uncons2 :: ByteString -> Maybe ((Word8, Word8), ByteString)
uncons2 xs = do 
    (x1, xs2) <- BS.uncons xs 
    (x2, xs3) <- BS.uncons xs2
    return ((x1, x2), xs3)

uncons3 :: ByteString -> Maybe ((Word8, Word8, Word8), ByteString)
uncons3 xs = do 
    ((x1, x2), xs2) <- uncons2 xs 
    (x3, xs3) <- BS.uncons xs2
    return ((x1, x2, x3), xs3)

lookupB64 :: Word8 -> Char 
lookupB64 x = encTable ! fromIntegral x
    where 
        encTable :: IntMap Char 
        encTable = fromList $ zip [0..] (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/'])

from3Bytes :: Word8 -> Word8 -> Word8 -> String 
from3Bytes x1 x2 x3 = map lookupB64 [y1, y2, y3, y4]
    where 
        conc :: Word32
        conc = fromIntegral x1 `shift` 16 + fromIntegral x2 `shift` 8 + fromIntegral x3
        y1 :: Word8
        y1 = fromIntegral $ conc `shiftR` 18 .&. 0x3f -- 0x3f = 0b111111
        y2 :: Word8 
        y2 = fromIntegral $ conc `shiftR` 12 .&. 0x3f
        y3 :: Word8
        y3 = fromIntegral $ conc `shiftR` 6 .&. 0x3f
        y4 :: Word8 
        y4 = fromIntegral $ conc .&. 0x3f

from2Bytes :: Word8 -> Word8 -> String 
from2Bytes x1 x2 = map lookupB64 [y1, y2, y3] ++ "="   -- one padding symbol
    where 
        conc :: Word32
        conc = (fromIntegral x1 `shift` 8 + fromIntegral x2) `shift` 2   -- two zeros to make 18 bits out of it
        y1 :: Word8 
        y1 = fromIntegral $ conc `shiftR` 12 .&. 0x3f
        y2 :: Word8 
        y2 = fromIntegral $ conc `shiftR` 6 .&. 0x3f
        y3 :: Word8
        y3 = fromIntegral $ conc .&. 0x3f

from1Bytes :: Word8 -> String 
from1Bytes x1 = map lookupB64 [y1, y2] ++ "=="  -- two padding symbols
    where
        conc :: Word32 
        conc = fromIntegral x1 `shift` 4 -- four zeros
        y1 :: Word8
        y1 = fromIntegral $ conc `shiftR` 6 .&. 0x3f
        y2 :: Word8
        y2 = fromIntegral $ conc .&. 0x3f

encode :: ByteString -> String
encode xs = case uncons3 xs of
  Just ((x1, x2, x3), xs2) -> from3Bytes x1 x2 x3 ++ encode xs2
  Nothing -> case uncons2 xs of
    Just ((x1, x2), xs2) -> from2Bytes x1 x2 ++ encode xs2
    Nothing -> case BS.uncons xs of
      Just (x1, xs2) -> from1Bytes x1 ++ encode xs2
      Nothing -> ""

main :: IO () 
main = print (encode inp) 
    where
        inp = parseHexInp' "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"