{-# LANGUAGE RecordWildCards #-}

module Test.Complexity.Pretty ( prettyStats
                              , printStats
                              ) where

import Text.PrettyPrint
import Text.Printf (printf)

import Test.Complexity.Base ( MeasurementStats(..)
                            , Sample
                            , Stats(..)
                            )

prettyStats :: MeasurementStats -> Doc
prettyStats (MeasurementStats {..}) =   text "desc:" <+> text msDesc
                                    $+$ text ""
                                    $+$ vcat [(ppSample' ("inputSize",["samples","min","mean2","max","stdDev"]))]
                                    $+$ vcat (map ppSample msSamples)
    where ppSample :: Sample -> Doc
          ppSample (x, y) = (text . printf "%10i") x <+> char '|' <+> ppStats y
          ppStats (Stats {..}) = ((text . printf "%8i") (length statsSamples))
                                 <+> hsep (map (text . printf "%8.3f")
                                               [statsMin, statsMean2, statsMax, statsStdDev]
                                          )
          ppSample' (x, y) = (text . printf "%10s") x <+> char '|' <+> ppStats' y
          ppStats' xs = hsep (map (text . printf "%8s")
                                xs
                                )

printStats :: [MeasurementStats] -> IO ()
printStats = mapM_ (\s -> do putStrLn . render . prettyStats $ s
                             putStrLn ""
                   )
