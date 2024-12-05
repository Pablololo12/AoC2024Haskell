module Day1a (solve) where

import qualified Data.Text as T
import qualified Data.List as L

parsn :: T.Text -> Int
parsn x = read (T.unpack x)

solve :: T.Text -> Int
solve x = sum $ map (\(q,w) -> abs (q-w)) $ zip (L.sort j) (L.sort jj)
    where
        (j, jj) = unzip $ map (\(w:ww:_) -> ((parsn w), (parsn ww))) $ map (T.words) $ T.lines x
