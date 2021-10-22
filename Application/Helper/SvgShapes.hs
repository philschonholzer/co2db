module Application.Helper.SvgShapes (arc, SvgPoint (..)) where

import Application.Helper.Co2Calculation
import IHP.Prelude
import Text.Printf
import qualified Prelude

data SvgPoint = SvgPoint {x :: Double, y :: Double}

instance Show SvgPoint where
  show (SvgPoint x y) = printf "%.1F" x <> ", " <> printf "%.1F" y

polarToCartesian :: SvgPoint -> Double -> Double -> SvgPoint
polarToCartesian (SvgPoint x y) radius angleInDegrees = SvgPoint (x + radius * cos angleInRadians) (y + radius * sin angleInRadians)
  where
    angleInRadians = (angleInDegrees - 90) * pi / 180

arc :: SvgPoint -> Double -> Double -> Double -> Text
arc centerPoint radius startAngle endAngle =
  unwords ["M", tshow start, "A", tshow radius, tshow radius, "0", largeArcFlag, "0", tshow end]
  where
    start = polarToCartesian centerPoint radius endAngle
    end = polarToCartesian centerPoint radius startAngle
    largeArcFlag = if endAngle - startAngle <= 180 then "0" else "1"
