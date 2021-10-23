module Application.Helper.Co2Calculation where

import Application.Helper.Average
import IHP.Prelude

averageCo2Value sources = average $ perUnit <$> sources
  where
    perUnit = calcCo2PerUnit <$> get #gCo2e <*> get #per

calcCo2PerUnit :: Double -> Double -> Double
calcCo2PerUnit = (/)

calcCo2PerConsumption :: Double -> Double -> Double -> Double
calcCo2PerConsumption gCo2e per consumption = gCo2e / per * consumption

calcCo2PerYear :: Double -> Double -> Double
calcCo2PerYear timesPerYear gCo2ePerConsumption = timesPerYear * gCo2ePerConsumption

partsOfOnePersonFootPrintPerYear :: Double -> Double
partsOfOnePersonFootPrintPerYear = flip (/) averageFootPrintPerYearPerPerson
  where
    averageFootPrintPerYearPerPerson = 6000000.0
