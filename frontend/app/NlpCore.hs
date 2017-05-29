{-# LANGUAGE OverloadedStrings #-}
module NlpCore where

import qualified Data.Map    as M
import           Data.Monoid (Sum, (<>))
import           Data.Text   (Text)
import qualified Data.Text   as T
import Data.Maybe
import Data.List (sortBy)
import Data.Ord (comparing)

type TextMap a = M.Map Text a

showTextSortMostFrequent :: Ord a => ((Text, a) -> Text) -> TextMap a -> [Text]
showTextSortMostFrequent showFun = map showFun . sortByFreq . M.toList
  where
    sortByFreq = sortBy (flip $ comparing snd)
    -- showWordAccum k v = [T.concat [k, ": ", T.pack . showFun $ v, "\n"]]

showTextMap :: (a -> String) -> TextMap a -> [Text]
showTextMap showFun = M.foldMapWithKey showWordAccum
  where showWordAccum k v = [T.concat [k, ": ", T.pack . showFun $ v, "\n"]]


------------------- rewrites

newtype Rewrite = Rw { doRw :: Text -> Text}

rwErase :: Text -> Rewrite
rwErase patt = rwReplace patt ""

rwReplace :: Text -> Text -> Rewrite
rwReplace patt new = Rw $ T.replace patt new

textToWords :: [Rewrite] -> Text -> [Text]
textToWords rewrites = filter (/= T.empty)
                     . T.split (`elem` splitChars)
                     . T.filter (`notElem` badChars)
                     . applyRewrites rewrites
  where badChars = [',', '.', '\"', ';', ':', ')', '(']
        splitChars = [' ', '\n', '\t']
        applyRewrites rws txt = foldl (flip doRw) txt rws


-- parses valid rewrite rules from lines of Text input.
parseRewrites :: Text -> [Rewrite]
parseRewrites = mapMaybe parseRewrite . T.lines
  where
    parseRewrite rule
    -- erasure rules
      | "~" `T.isPrefixOf` rule = let rwe = T.splitOn "~" rule
                                  in case length rwe of
          2 -> if head rwe == "" && rwe !! 1 /= "" then
                 Just $ rwErase $ rwe !! 1
               else
                 Nothing
          _ -> Nothing
    -- replacement rules
      | otherwise =let rw = filter (not . T.null ) . T.splitOn ":=" $ rule
                   in case length rw of
          2 -> Just $ rwReplace (head rw) (rw !! 1)
          _ -> Nothing

---------------------------------------
wordsAccum :: Monoid a =>
              TextMap a       -- starting textmap
           -> [Text]          -- words to be accumulated
           -> (Text -> Text)  -- you can pass word transformator
           -> (Text -> a)     -- function generating value for mappend
           -> TextMap a       -- resulting textmap
wordsAccum tm words' keyFun valueFun = foldl accumWord tm words'
  where
    accumWord m w = M.insertWithKey insertOrAdd (keyFun w) (valueFun w) m
    insertOrAdd _ old new = old <> new


defaultWordsCount :: Bool                    -- case sensitivity
                  -> [Rewrite]               -- rewrites
                  -> TextMap (Sum Integer)   -- starting textmap
                  -> Text                    -- new text
                  -> TextMap (Sum Integer)   -- resulting textmap
defaultWordsCount cs rewrites tm t =
  wordsAccum tm ((if cs then id else map T.toLower) . textToWords rewrites $ t) id (const 1)
