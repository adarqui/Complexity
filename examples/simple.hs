{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Function      (fix)
import Data.List          (sort, unfoldr)
import System.Environment (getArgs)
import qualified Data.List   as L
import qualified Data.Map    as M
import qualified Data.IntMap as IM

import Test.Complexity

-------------------------------------------------------------------------------
-- Some input generators for lists of Int

genIntList :: InputGen [Int]
genIntList n = let n' = fromInteger n
              in [n', n' - 1 .. 0]

-- Very simple pseudo random number generator.
pseudoRnd :: Int -> Int -> Int -> Int -> [Int]
pseudoRnd p1 p2 n d = iterate (\x -> (p1 * x + p2) `mod` n) d

genIntList2 :: InputGen [Int]
genIntList2 n = take (fromInteger n) $ pseudoRnd 16807 0 (2 ^ (31 :: Int) - 1) 79


-------------------------------------------------------------------------------
-- Big disparity

nums0 :: Integer -> Integer
nums0 n = n

nums1 :: Integer -> Integer
nums1 = fib0

expNums :: [Experiment]
expNums = [ pureExperiment "nums0" (cpuTimeSensor 10) id nums0
          , pureExperiment "nums1" (cpuTimeSensor 10) id nums1
          ]



-------------------------------------------------------------------------------
-- Bunch of sums

sumFoldl :: (Num a) => [a] -> a
sumFoldl xs = foldl (+) 0 xs

sumFoldl' ::  (Num a) => [a] -> a
sumFoldl' xs = L.foldl' (+) 0 xs

sumFoldr ::  (Num a) => [a] -> a
sumFoldr xs = L.foldr (+) 0 xs

expSums :: [Experiment]
expSums   = [
  pureExperiment "foldl" (cpuTimeSensor 10) genIntList2 sumFoldl,
  pureExperiment "foldl'" (cpuTimeSensor 10) genIntList2 sumFoldl',
  pureExperiment "foldr" (cpuTimeSensor 10) genIntList2 sumFoldr,
  pureExperiment "sum" (cpuTimeSensor 10) genIntList2 sum
 ]

-------------------------------------------------------------------------------
-- Bunch of fibonacci functions

fib0 :: Integer -> Integer
fib0 0 = 0
fib0 1 = 1
fib0 n = fib0 (n - 1) + fib0 (n - 2)

fib1 :: Integer -> Integer
fib1 0 = 0
fib1 1 = 1
fib1 n | even n         = f1 * (f1 + 2 * f2)
       | n `mod` 4 == 1 = (2 * f1 + f2) * (2 * f1 - f2) + 2
       | otherwise      = (2 * f1 + f2) * (2 * f1 - f2) - 2
   where k = n `div` 2
         f1 = fib1 k
         f2 = fib1 (k-1)

fib2 :: Integer -> Integer
fib2 n = fibs !! fromInteger n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib3 :: Integer -> Integer
fib3 n = fibs !! fromInteger n
    where fibs = scanl (+) 0 (1:fibs)

fib4 :: Integer -> Integer
fib4 n = fibs !! fromInteger n
    where fibs = fix (scanl (+) 0 . (1:))

fib5 :: Integer -> Integer
fib5 n = fibs !! fromInteger n
    where fibs = unfoldr (\(a,b) -> Just (a,(b, a+b))) (0,1)

fib6 :: Integer -> Integer
fib6 n = fibs !! fromInteger n
    where fibs = map fst $ iterate (\(a,b) -> (b, a+b)) (0,1)

expFibs :: [Experiment]
expFibs = [ pureExperiment "fib0" (cpuTimeSensor 10) id fib0
          , pureExperiment "fib1" (cpuTimeSensor 10) id fib1
          , pureExperiment "fib2" (cpuTimeSensor 10) id fib2
          , pureExperiment "fib3" (cpuTimeSensor 10) id fib3
          , pureExperiment "fib4" (cpuTimeSensor 10) id fib4
          , pureExperiment "fib5" (cpuTimeSensor 10) id fib5
          , pureExperiment "fib6" (cpuTimeSensor 10) id fib6
          ]

-------------------------------------------------------------------------------
-- Sorting algorithms

bsort :: Ord a => [a] -> [a]
bsort [] = []
bsort xs = iterate swapPass xs !! (length xs - 1)
   where swapPass (x:y:zs) | x > y     = y : swapPass (x:zs)
                           | otherwise = x : swapPass (y:zs)
         swapPass xs = xs

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs)

expBSort, expQSort, expSort, expSorts :: [Experiment]
expBSort  = [pureExperiment "bubble sort"    (cpuTimeSensor 10) genIntList2 bsort]
expQSort  = [pureExperiment "quick sort"     (cpuTimeSensor 10) genIntList2 qsort]
expSort   = [pureExperiment "Data.List.sort" (cpuTimeSensor 10) genIntList2 sort]
expSorts  = expBSort ++ expQSort ++ expSort

-------------------------------------------------------------------------------
-- Map lookups

mkMap :: InputSize -> (Int, M.Map Int Int)
mkMap n = let n' = fromInteger n
          in (n' `div` 2, M.fromList [(k, k) | k <- [0 .. n']])

mkIntMap :: InputSize -> (Int, IM.IntMap Int)
mkIntMap n = let n' = fromInteger n
             in (n' `div` 2, IM.fromList [(k, k) | k <- [0 .. n']])

expMaps :: [Experiment]
expMaps = [ pureExperiment "Data.Map pure" (cpuTimeSensor 10) mkMap    (uncurry M.lookup)
          , pureExperiment "Data.IntMap"   (cpuTimeSensor 10) mkIntMap (uncurry IM.lookup)
          ]

-------------------------------------------------------------------------------

cmdLine :: String -> [Experiment] -> IO ()
cmdLine msg xs = do
                banner msg
                args <- getArgs
                if length args == 2
                  then let (a1:a2:_) = take 2 args
                           maxTime   = (read a1) :: Double
                           maxN      = (read a2) :: InputSize
                       in do
                          putStrLn "SmartMeasure:"
                          simpleSmartMeasure 1.1 maxN 10 maxTime xs
                          putStrLn "MeasureNs:"
                          simpleMeasureNs [1..maxN] 10 maxTime xs
                  else putStrLn "Error: I need 2 arguments (max time and max input size)"
 where
  banner msg = putStrLn $ "-------------------------: " ++ msg

main :: IO ()
main = do
 cmdLine "nums" expNums
 cmdLine "sums" expSums
 cmdLine "sorts" expSorts
 cmdLine "maps" expMaps
 cmdLine "fibs" expFibs
