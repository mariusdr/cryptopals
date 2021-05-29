module Main where 
import HexParser (decodeHex, encodeHex)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Bits as B
import Data.Word (Word8)
import Data.Char (chr, ord)
import Data.Map (Map, unionsWith, singleton, (!), member)
import Data.List (head, lines, sortBy)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode), hGetLine) 
import Control.Monad.State
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C8L
import Data.Either (fromRight)
import Data.Function (on)

import LanguageScores (oneGramScore, oneGramScoreStr, nonAsciiPenalty)

-- main :: IO ()
-- main = do 
--     let inp = "helloworld"
--     let dec = encodeSingleByteXor 42 $ BSU.fromString inp
--     let enc = decodeSingleByteXor 42 dec
--     print dec
--     print enc

xor :: ByteString -> ByteString -> ByteString 
xor xs ys = BS.pack $ map (uncurry B.xor) $ BS.zip xs ys

-- Set 1 Challenge 2
testXor :: IO ()
testXor = do 
    let inp1 = decodeHex "1c0111001f010100061a024b53535009181c"
    let inp2 = decodeHex "686974207468652062756c6c277320657965"
    let res = xor inp1 inp2 
    print "Result: "
    print res
    print "Hex Encoded Result: "
    print (encodeHex res)
    print "Target: " 
    print "746865206b696420646f6e277420706c6179"

repeatByte :: Int -> Word8 -> ByteString 
repeatByte n x = BS.pack $ replicate n x

encodeSingleByteXor :: Word8 -> ByteString -> ByteString 
encodeSingleByteXor key msg = xor msg (repeatByte (BS.length msg) key)

decodeSingleByteXor :: Word8 -> ByteString -> ByteString 
decodeSingleByteXor = encodeSingleByteXor


data Solution = Solution {
    key :: Word8, 
    message :: ByteString, 
    score :: Float 
    } 

instance Eq Solution where 
    (==) (Solution _ _ s1) (Solution _ _ s2) = s1 == s2

instance Ord Solution where 
    compare (Solution _ _ s1) (Solution _ _ s2) = compare s1 s2

instance Show Solution where 
    show sol = "Solution {score = " ++ show (score sol) ++ 
               " key = " ++ [(chr . fromIntegral) (key sol)] ++ 
               " message = " ++ show (message sol) ++ "}"

decodeAndScoreSingleByteXor :: (ByteString -> Float) -> ByteString -> Word8 -> Solution
decodeAndScoreSingleByteXor scoreFn msg key  = Solution key dec score
    where 
        dec = decodeSingleByteXor key msg 
        score = scoreFn dec

solveSingleByteXor :: (ByteString -> Float) -> ByteString -> [Word8] -> [Solution]
solveSingleByteXor scoreFn msg keys = sortBy (flip compare) solutions
    where 
        solutions = map (decodeAndScoreSingleByteXor scoreFn msg) keys 

-- set 1 challenge 3
breakSingleByteXor :: IO ()
breakSingleByteXor = do 
    let msg = decodeHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    
    let keyRange = [0..255]
    let scoreFn = oneGramScore 
    let solutions = solveSingleByteXor scoreFn msg keyRange

    -- print the 10 solutions with the highest scores
    mapM_ print $ take 10 solutions

solveSingleByteXorMultiLine :: (ByteString -> Float) -> [(Int, ByteString)] -> [Word8] -> [(Int, Solution)]
solveSingleByteXorMultiLine scoreFn msgs keys = sortBy (\(_, s0) (_, s1) -> compare s1 s0) allSolutions
    where 
        solutionsOfLine i msg = map (\m -> (i, m)) solSet
            where 
                solSet = map (decodeAndScoreSingleByteXor scoreFn msg) keys 

        allSolutions = concatMap (uncurry solutionsOfLine) msgs

solveSingleByteXorMultiLineTakeN :: Int -> (ByteString -> Float) -> [(Int, ByteString)] -> [Word8] -> [(Int, Solution)]
solveSingleByteXorMultiLineTakeN n scoreFn msgs keys = take n $ solveSingleByteXorMultiLine scoreFn msgs keys

-- set 1 challenge 4
detectSingleByteXor :: IO ()
detectSingleByteXor = do
    handle <- openFile "set1challenge4input" ReadMode
    conts <- hGetContents handle
    let inputs = zip [0..(length (lines conts))] (map decodeHex (lines conts))

    let keyRange = [0..255]
    let scoreFn = oneGramScore

    let solutions = solveSingleByteXorMultiLineTakeN 10 scoreFn inputs keyRange 

    -- mapM_ (putStrLn . (\(idx, line) -> show idx ++ ": " ++ show line)) inputs
    mapM_ (putStrLn . (\(idx, sol) -> show idx ++ ": " ++ show sol)) solutions

main = detectSingleByteXor



-- repeatUntil :: Int -> ByteString -> ByteString 
-- repeatUntil = repeatUntil' BS.empty 
--     where 
--         repeatUntil' :: ByteString -> Int -> ByteString -> ByteString 
--         repeatUntil' acc n bs | BS.length acc < n = repeatUntil' (BS.concat [acc, bs]) n bs
--                               | BS.length acc >= n = BS.take n acc 

-- repeatedKeyXorCipher :: ByteString -> ByteString -> ByteString  
-- repeatedKeyXorCipher key input = xor key' input
--     where
--         key' = repeatUntil (BS.length input) key


-- hammingDist :: ByteString -> ByteString -> Int 
-- hammingDist xs ys = sum $ map B.popCount $ BS.unpack $ xor xs ys


-- selfHammingDist :: ByteString -> Int -> Int 
-- selfHammingDist xs keySize = hammingDist fst snd 
--     where 
--         fst = BS.take keySize xs 
--         snd = BS.take keySize $ BS.drop (BS.length xs - keySize) xs 


-- estimateKeySize :: ByteString -> [(Int, Float)] 
-- estimateKeySize xs = sortBy (compare `on` snd) $ zip [2..40] scores 
--     where 
--         scores = map (\k -> fromIntegral (selfHammingDist xs k) / fromIntegral k) [2..40]


-- blockSplit :: Int -> ByteString -> [ByteString] 
-- blockSplit = blockSplit' []
--     where 
--         blockSplit' :: [ByteString] -> Int -> ByteString -> [ByteString]
--         blockSplit' acc blockSize xs | BS.null xs = acc
--                                      | otherwise = blockSplit' (acc ++ [BS.take blockSize xs]) blockSize (BS.drop blockSize xs) 

-- transposeBlocks :: [ByteString] -> [ByteString]
-- transposeBlocks xss = map BS.pack tSplits
--     where 
--         transpose :: [[Word8]] -> [[Word8]]
--         transpose ([]:_) = []
--         transpose x = map head x : transpose (map tail x)
        
--         padMat :: Int -> [[Word8]] -> [[Word8]]
--         padMat reqRowLen = map appendFn
--             where 
--                 delta :: [Word8] -> Int
--                 delta xs = reqRowLen - length xs

--                 appendFn :: [Word8] -> [Word8]
--                 appendFn xs = if delta xs > 0 then xs ++ replicate (delta xs) 0 else xs

--         maxLen = maximum (map BS.length xss)
--         splits = map BS.unpack xss
--         tSplits = transpose $ padMat maxLen splits

-- -- main :: IO ()
-- -- main = print $ hammingDist a b 
-- --     where 
-- --         a = encodeUtf8 $ T.pack "this is a test"
-- --         b = encodeUtf8 $ T.pack "wokka wokka!!!"

-- -- main :: IO ()
-- -- main = do 
-- --     let bss = [BS.pack [1,2,3,4], BS.pack [5,6,7,8]]
-- --     let tBss = transposeBlocks bss
-- --     let ttBss = transposeBlocks tBss
-- --     print $ (map BS.unpack) tBss
-- --     print $ (map BS.unpack) ttBss

-- -- main :: IO () 
-- -- main = do 
-- --     handle <- openFile "set1challenge6input" ReadMode 
-- --     txt <- hGetContents handle 
-- --     let msg = fromRight C8.empty $ B64.decode $ C8.filter (/= '\n') $ C8.pack txt
-- --     let keySizes = estimateKeySize msg
-- --     let (keySize, _) = head keySizes

-- --     let blocks = blockSplit keySize msg
-- --     let transposedBlocks = transposeBlocks blocks

-- --     let solvedTransposedBlocks = map (solve) 
-- --     --print $ map BS.unpack bs

-- --     hClose handle


