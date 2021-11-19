module Application.Helper.View (module Application.Helper.View, module Application.Helper.Co2Calculation, module Application.Helper.SvgShapes) where

import Application.Helper.Co2Calculation
import Application.Helper.SvgShapes
import IHP.ViewPrelude
import Text.Blaze.Html5 (preEscapedString, preEscapedToMarkup)
import Text.Printf (printf)

formatDecimal :: Double -> String
formatDecimal amount
  | amount == 0 = "0.00"
  | amount < 0.0005 = "~ 0.000"
  | amount < 0.001 = "0.001"
  | amount < 0.1 = amount |> printf "%2.3F"
  | amount < 1 = amount |> printf "%2.3F"
  | amount < 10 = amount |> printf "%01.2F"
  | amount < 100 = amount |> printf "%02.1F"
  | otherwise = amount |> printf "%03.0F"
