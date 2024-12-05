module Day2a (solve) where

import qualified Data.Text as T
import qualified Data.List as L

parsn :: T.Text -> Int
parsn x = read (T.unpack x)

madness :: [Int] -> Bool
madness a = ((all (<0) jum) || (all (>0) jum)) && jumps
    where
        jumps = all (\x -> ((abs x) <= 3) && x/=0) jum
        jum = map (\(x,y) -> x-y) $ zip (init a) (drop 1 a)

solve :: T.Text -> Int
solve x = length $ filter (madness) $ map (\y -> map (parsn) $ T.words y) $ T.lines x
