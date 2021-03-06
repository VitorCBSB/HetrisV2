{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( tupleHistogram,
    fisherYatesShuffle,
    timerText,
  )
where

import Data.Fixed
import Data.Function (on)
import qualified Data.List as List
import Data.Map
import qualified Data.Text as T
import System.Random

-- Returns the histogram of a list of tuples, e.g.,
--
-- tupleHistogram [(0, 1), (0, 2), (0, 3), (1, 1)] = [(0, [1, 2, 3]), (1, [1])]
tupleHistogram :: [(Int, b)] -> [(Int, [b])]
tupleHistogram xs =
  let groupedElems = List.groupBy (\a b -> fst a == fst b) $ List.sortBy (compare `on` fst) xs
   in List.map (List.foldl' (\(_, l) a -> (fst a, l ++ [snd a])) (0, [])) groupedElems

fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYatesShuffle :: RandomGen g => g -> [a] -> ([a], g)
fisherYatesShuffle gen [] = ([], gen)
fisherYatesShuffle gen (l : ls) =
  toElems $ List.foldl' fisherYatesStep (initial l gen) (numerate ls)
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1 ..]
    initial x gen = (singleton 0 x, gen)

-- Given a positive double representing a time in seconds,
-- return a Text that formats it like M+:ss.mm
timerText :: Double -> T.Text
timerText time =
  minutes <> ":" <> seconds <> "." <> milliseconds
  where
    minutes = (T.pack . show) (floor (time / 60.0))
    actualSeconds = floor $ mod' time 60
    seconds = (if actualSeconds < 10 then "0" else "") <> (T.pack . show) actualSeconds
    actualMilliseconds = time - fromIntegral (floor time)
    firstDigitMilli = floor (10 * actualMilliseconds)
    secondDigitMilli = floor (10 * ((10 * actualMilliseconds) - fromIntegral (floor (10 * actualMilliseconds))))
    milliseconds = (T.pack . show) firstDigitMilli <> (T.pack . show) secondDigitMilli