{-|
Some utilities to quickly perform experiments.
-}

module Test.Complexity.Utils
    ( quickPerformExps
    , simpleMeasureNs
    , simpleSmartMeasure
    ) where

import Test.Complexity.Base   ( MeasurementStats
                              , Experiment
                              , InputSize
                              , performExperiment
                              , inputSizeFromList
                              , simpleLinearHeuristic
                              )
--import Test.Complexity.Chart  (showStatsChart)
import Test.Complexity.Pretty (printStats)


quickPerformExps :: (a -> IO MeasurementStats) -> [a] -> IO ()
quickPerformExps f xs = do stats <- mapM f xs
                           printStats     stats
--                           showStatsChart stats

simpleMeasureNs :: [InputSize] -> Integer -> Double -> [Experiment] -> IO ()
simpleMeasureNs ns numSamples maxTime =
    quickPerformExps (performExperiment (inputSizeFromList ns) numSamples maxTime)


simpleSmartMeasure :: Double -> InputSize -> Integer -> Double -> [Experiment] -> IO ()
simpleSmartMeasure step maxN numSamples maxTime xs =
    let tMax = maxTime / (fromIntegral $ length xs)
    in quickPerformExps (performExperiment (simpleLinearHeuristic step maxN) numSamples tMax) xs
