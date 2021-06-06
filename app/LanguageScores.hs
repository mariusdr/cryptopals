module LanguageScores where 

import Data.HashMap.Strict (HashMap, unionWith, foldrWithKey, findWithDefault, fromList, empty, singleton)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Char (chr, ord, toLower)
import Data.Hashable (Hashable)

import Utils (unionsWith)

scoreHistograms :: (Eq t, Hashable t) => HashMap t Float -> HashMap t Int -> Float 
scoreHistograms scoreMap = foldrWithKey foldF 0 
    where 
        foldF key val acc = fromIntegral val * findWithDefault 0.0 key scoreMap + acc 


-- from http://norvig.com/mayzner.html
oneGramCounts :: HashMap Char Float 
oneGramCounts = fromList oneGrams 
    where 
        oneGrams = [
            ('e', 12.49), ('t', 9.28), ('a', 8.04), ('o', 7.64), ('i', 7.57), ('n', 7.23),
            ('s', 6.51), ('r', 6.28), ('h', 5.05), ('l', 4.07), ('d', 3.82), ('c', 3.34),
            ('u', 2.73), ('m', 2.51), ('f', 2.40), ('p', 2.14), ('g', 1.87), ('w', 1.68),
            ('y', 1.66), ('b', 1.48), ('v', 1.05), ('k', 0.54), ('x', 0.23), ('j', 0.16),
            ('q', 0.12), ('z', 0.09)]

oneGramHistogram :: String -> HashMap Char Int 
oneGramHistogram xs = unionsWith (+) $ map (flip singleton 1) xs 

singleCharScore :: HashMap Char Float -> (String -> String) -> String -> Float 
singleCharScore charScoreMap normalizeFn xs = scoreHistograms charScoreMap $ oneGramHistogram $ normalizeFn xs

oneGramScoreStr :: String -> Float 
oneGramScoreStr = singleCharScore  oneGramCounts (map toLower)

oneGramScore :: ByteString -> Float 
oneGramScore = oneGramScoreStr . BSU.toString 

nonAsciiPenalty :: Float -> ByteString -> Float 
nonAsciiPenalty penalty xs = sum $ map score (BS.unpack xs)
    where 
        asciiRange = [65..90] ++ [97..122]
        score x = if x `notElem` asciiRange then penalty else 0.0


