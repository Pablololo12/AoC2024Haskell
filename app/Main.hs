module Main where

import Data.List as L
import Data.Text
import Data.Maybe
import qualified Data.Text.IO
import Distribution.System
import System.Environment

import Day1a
import Day1b
import Day2a
import Day2b

retdiv :: OS -> String
retdiv Windows = "\\"
retdiv _ = "/"

pathJoin :: [String] -> String
pathJoin x = L.intercalate j x
    where j = retdiv buildOS

days :: [(Text->Int,String,String,Int)]
days = [(Day1a.solve, "day1.txt","Day1a",2192892),
    (Day1b.solve, "day1.txt","Day1b",22962826),
    (Day2a.solve, "day2.txt","Day2a",269),
    (Day2b.solve, "day2.txt","Day2b",337)]


run :: (Text->Int, String,String,Int) -> IO ()
run (f,s,d,i) = do
            content <- Data.Text.IO.readFile $ pathJoin ["inputs",s]
            let res = f content
            print $ d ++ " " ++ (show res) ++ " " ++ (show $ res==i)

search :: String -> (Text->Int, String, String,Int)
search x = fromMaybe (L.head $ days) $ L.find (\(_,_,w,_) -> x==w) $ days

parseArgs :: [String] -> IO ()
parseArgs [] = mapM_ (run) $ days
parseArgs (x:_) = run $ search x

main :: IO ()
main = do
    args <- getArgs
    parseArgs args
