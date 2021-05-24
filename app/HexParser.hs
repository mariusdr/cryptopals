module HexParser where

import Data.Attoparsec.ByteString.Char8 (Parser, char8, choice, parseOnly)
import qualified Data.ByteString as BS
import Control.Applicative (liftA2, liftA3, many)
import Data.Bits (shift)
import Data.ByteString (ByteString, pack, uncons)
import Data.Char (ord)
import Data.IntMap.Strict (IntMap, fromAscList, (!))
import Data.Word (Word8)
import Data.Either (fromRight)


hex :: Parser Word8
hex = liftA2 mkWord8 hexChar hexChar 
    where 
        hexChar :: Parser Word8
        hexChar = choice $ map char8 $ ['0'..'9'] ++ ['a'..'f']

        mkWord8 :: Word8 -> Word8 -> Word8
        mkWord8 a b = (hexMap ! fromIntegral a) `shift` 4 + (hexMap ! fromIntegral b)
            where 
                hexMap :: IntMap Word8 
                hexMap = fromAscList $ zip (map ord $ ['0'..'9'] ++ ['a'..'f']) [0..]

hexString :: Parser ByteString
hexString = fmap pack (many hex)

parseHex :: ByteString -> Either String ByteString 
parseHex = parseOnly hexString

parseHexInp :: String -> Either String ByteString
parseHexInp = parseHex . BS.pack . map (fromIntegral . ord) 

parseHexInp' :: String -> ByteString  
parseHexInp' inp = fromRight zeroStr res
    where 
        res = parseHexInp inp 
        zeroStr = BS.pack [(fromIntegral . ord) '0']

