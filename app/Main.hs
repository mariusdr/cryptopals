module Main where 
import HexParser (decodeHex, encodeHex)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Bits as B
import Data.Word (Word8, byteSwap16)
import Data.Char (chr, ord)
import Data.Map (Map, unionsWith, singleton, (!), member)
import Data.List (head, lines, sortBy)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode), hGetLine) 
import Control.Monad.State ()
import Control.Monad ()
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C8L
import Data.Either (fromRight)
import Data.Function (on)

import LanguageScores (oneGramScore, oneGramScoreStr, nonAsciiPenalty)
import Utils (computeByteEntropy, xor, hammingDist, transposeByteMatrix)
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (BlockCipher (ecbEncrypt, ecbDecrypt), Cipher (cipherInit))
import Crypto.Error (throwCryptoError)

initAES :: ByteString -> AES128 
initAES key = throwCryptoError (cipherInit key)

-- set 1 challenge 7
encryptAESInECBMode :: IO ()
encryptAESInECBMode = do 
    handle <- openFile "set1challenge7input" ReadMode 
    txt <- hGetContents handle 
    let msg = fromRight C8.empty $ B64.decode $ C8.filter (/= '\n') $ C8.pack txt
    let key = BSU.fromString "YELLOW SUBMARINE"
    let decMsg = ecbDecrypt (initAES key) msg
    print decMsg

-- slice a bytestring in n byte blocks
sliceInBlocks :: Int -> ByteString -> [ByteString]
sliceInBlocks n msg 
    | BS.length msg == 0 = []
    | BS.length msg < n = [msg]
    | otherwise = h : sliceInBlocks n t
    where 
        (h, t) = BS.splitAt n msg

-- count how often msg occurs in msgs
countDups :: ByteString -> [ByteString] -> Int 
countDups msg msgs = sum $ map fromEnum $ map (== msg) msgs

countAllDups :: [ByteString] -> Int 
countAllDups msgs = sum (map (flip countDups msgs) msgs) - length msgs

solveForLine :: (Int, ByteString) -> IO ()
solveForLine (n, msg) = do 
    let blocks = sliceInBlocks 16 msg 
    putStrLn ("Message " ++ show n ++ " has " ++ show (countAllDups blocks) ++ " duplicate 16 Byte blocks.")

-- set 1 challenge 8
detectECBEncryptedMessage :: IO ()
detectECBEncryptedMessage = do 
    handle <- openFile "set1challenge8input" ReadMode
    txt <- hGetContents handle
    let msgs = map decodeHex (lines txt)
    mapM_ solveForLine (zip [0..] msgs)


pcks7 :: Int -> ByteString -> ByteString 
pcks7 k msg = BS.concat [msg, pad]
    where 
        padval = k - BS.length msg `mod` k
        diff = k - BS.length msg
        pad = BS.replicate diff (fromIntegral padval)

-- set 2 challenge 1
implPKCS7 :: IO ()
implPKCS7 = do 
    let msg = BSU.fromString "YELLOW SUBMARINE"
    let msg' = pcks7 20 msg
    print msg'

main = implPKCS7