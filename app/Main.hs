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
import Control.Monad.State
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C8L
import Data.Either (fromRight)
import Data.Function (on)

import LanguageScores (oneGramScore, oneGramScoreStr, nonAsciiPenalty)
import Utils (computeByteEntropy, xor, hammingDist, transposeByteMatrix)



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
    
    let keyRange = [48..57] ++ [65..90] ++ [97..122] -- ascii 0-9 A-Z a-z
    -- let keyRange = [0..255]
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

-- set 1 challenge 4
detectSingleByteXor :: IO ()
detectSingleByteXor = do
    handle <- openFile "set1challenge4input" ReadMode
    conts <- hGetContents handle
    let inputs = zip [0..(length (lines conts))] (map decodeHex (lines conts))

    -- Single byte xor cipher is invariant under entropy, so if there is only 
    -- one "real" sentence in here and the other ones are random generated (high entropy)
    -- the encrypted real sentence should still have the least entropy..
    -- I.e. only use the ten lowest entropy inputs:
    let inputsSorted = sortBy (\(_, m0) (_, m1) -> compare (computeByteEntropy m0) (computeByteEntropy m1)) inputs
    let inputsSelected = take 10 inputsSorted

    let keyRange = [0..255]
    let scoreFn = oneGramScore

    -- only return the 10 best solutions
    let solutions = take 10 $ solveSingleByteXorMultiLine scoreFn inputsSelected keyRange 

    -- mapM_ (putStrLn . (\(idx, line) -> show idx ++ ": " ++ show line)) inputs
    mapM_ (putStrLn . (\(idx, sol) -> show idx ++ ": " ++ show sol)) solutions

-- test optimization for set 1 challenge 4
entropyAnalysis :: IO ()
entropyAnalysis = do 
    handle <- openFile "set1challenge4input" ReadMode
    conts <- hGetContents handle
    let inputs = zip [0..(length (lines conts))] (map decodeHex (lines conts))

    let entropies = map (\(i, m) -> (i, computeByteEntropy m)) inputs
    let entropiesSorted = sortBy (compare `on` snd) entropies 

    mapM_ (putStrLn . (\(idx, sol) -> show idx ++ ": " ++ show sol)) entropiesSorted

encodeRepeatedKeyXor :: ByteString -> ByteString -> ByteString  
encodeRepeatedKeyXor key input = xor key' input
    where
        repeatUntil = repeatUntil' BS.empty 
            where 
                repeatUntil' acc n bs | BS.length acc < n = repeatUntil' (BS.concat [acc, bs]) n bs
                                      | BS.length acc >= n = BS.take n acc 
        
        key' = repeatUntil (BS.length input) key

decodeRepeatedKeyXor :: ByteString -> ByteString -> ByteString 
decodeRepeatedKeyXor = encodeRepeatedKeyXor

testRepeatedKeyXor :: IO () 
testRepeatedKeyXor = do 
    let inp = BSU.fromString "Burning 'em, if you ain't quick and nimble I go crazy when I hear a cymbal"
    let key = BSU.fromString "ICE"
    let enc = encodeRepeatedKeyXor key inp 
    print $ encodeHex enc


testHammingdist :: IO ()
testHammingdist = do
    let inp1 = BSU.fromString "this is a test"
    let inp2 = BSU.fromString "wokka wokka!!!"
    let dist = hammingDist inp1 inp2 
    print dist -- should be 37

splitMessage :: Int -> ByteString -> [ByteString] 
splitMessage = split' []
    where
        split' acc blockSize xs | BS.null xs = acc
                                | otherwise = split' (acc ++ [BS.take blockSize xs]) blockSize (BS.drop blockSize xs) 

scoreKeySize :: Int -> ByteString -> Float
scoreKeySize keySize msg = sum scores / fromIntegral (length scores)
    where 
        blocks = splitMessage keySize msg 
        blockIter = zip blocks (tail blocks)
        scoreFn b1 b2 = fromIntegral (hammingDist b1 b2) / fromIntegral keySize
        scores = map (uncurry scoreFn) blockIter 

estimateKeySize :: [Int] -> ByteString -> [(Int, Float)] 
estimateKeySize keySizes msg = sortBy (compare `on` snd) scores 
    where 
        scores = map (\ks -> (ks, scoreKeySize ks msg)) keySizes

transposeBlocks :: [ByteString] -> [ByteString]
transposeBlocks messages = map BS.pack tSplits
    where 
        padMat :: Int -> [[Word8]] -> [[Word8]]
        padMat reqRowLen = map appendFn
            where 
                delta xs = reqRowLen - length xs
                appendFn xs = if delta xs > 0 then xs ++ replicate (delta xs) 0 else xs

        maxLen = maximum (map BS.length messages)
        splits = map BS.unpack messages
        tSplits = transposeByteMatrix $ padMat maxLen splits

solveForKeySize :: Int -> ByteString -> ByteString
solveForKeySize keySize msg = BS.pack keys
    where 
        blocks = splitMessage keySize msg
        transposedBlocks = transposeBlocks blocks 

        solveForBlock :: ByteString -> Word8 
        solveForBlock block = key $ head solutions
            where 
                -- keyRange = [0..255]
                keyRange = [48..57] ++ [65..90] ++ [97..122] -- ascii 0-9 A-Z a-z
                scoreFn = oneGramScore 
                solutions = solveSingleByteXor scoreFn msg keyRange
        
        keys = map solveForBlock transposedBlocks


main :: IO () 
main = do 
    handle <- openFile "set1challenge6input" ReadMode 
    txt <- hGetContents handle 
    let msg = fromRight C8.empty $ B64.decode $ C8.filter (/= '\n') $ C8.pack txt
    let keySizes = estimateKeySize [1..40] msg

    -- mapM_ print keySizes

    let key = solveForKeySize 29 msg
    let dec = decodeRepeatedKeyXor key msg

    -- print ("Key: " ++ encodeHex key) 
    -- print ("Decoded Message: "  ++ BSU.toString dec)

    hClose handle


