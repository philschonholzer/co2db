module Application.Helper.Co2Calculation where

import Application.Helper.Average
import IHP.Prelude

averageCo2Value sources = average $ perUnit <$> sources
  where
    perUnit = calcCo2PerUnit <$> get #gCo2e <*> get #per

calcCo2PerUnit :: Double -> Double -> Double
calcCo2PerUnit = (/)

calcCo2Factor :: Double -> Double -> Double -> Double
calcCo2Factor gCo2e per consumption = gCo2e / per * consumption

partsOfOnePersonFootPrintPerYear :: Double -> Double
partsOfOnePersonFootPrintPerYear = flip (/) averageFootPrintPerYearPerPerson
  where
    averageFootPrintPerYearPerPerson = 6000.0
