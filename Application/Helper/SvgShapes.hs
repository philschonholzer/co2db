module Application.Helper.SvgShapes (segmentMask, partsToDeg, SvgPoint (..)) where

import Application.Helper.Co2Calculation
import Data.Fixed
import IHP.Prelude
import IHP.ViewPrelude
import Text.Printf
import qualified Prelude

data SvgPoint = SvgPoint {x :: Double, y :: Double}

data SemiCircle = LeftSemiCircle | RightSemiCircle deriving (Eq)

instance Show SvgPoint where
  show (SvgPoint x y) = printf "%.1F" x <> ", " <> printf "%.1F" y

segmentMask :: SvgPoint -> Double -> Double -> Double -> Html
segmentMask centerPoint size startAngle endAngle =
  [hsx|
    <defs>
      <mask id="segmentMask">
        <polygon points={halfSqureMask} fill="white" transform={"rotate("<> tshow (startAngle) <>" "<>tshow centerPoint<>")"}/>
        <polygon points={halfSqureMask} fill={fillMask} transform={"rotate("<> tshow (endAngleMask) <>" "<>tshow centerPoint<>")"}/>
      </mask>
    </defs>
  |]
  where
    halfSqureMask :: Text
    halfSqureMask =
      halfSize -- top left
        <> ",-"
        <> halfSize
        <> " " -- top right
        <> tripleSize
        <> ",-"
        <> halfSize
        <> " " -- bottom rigth
        <> tripleSize
        <> ","
        <> tripleSize
        <> " " -- bottom left
        <> halfSize
        <> ","
        <> tripleSize
        <> " "

    -- Everything under a white pixel will be visible in the mask
    -- Everything under a black pixel will be invisible in the mask
    fillMask :: Text
    fillMask
      | isLeftOrRightSemiCircle == RightSemiCircle = "black"
      | otherwise = "white"

    -- Rotate the end angle by 180° if it flips from hiding "black" to showing "white"
    endAngleMask :: Double
    endAngleMask
      | isLeftOrRightSemiCircle == RightSemiCircle = endAngle
      | otherwise = endAngle - 180

    isLeftOrRightSemiCircle :: SemiCircle
    isLeftOrRightSemiCircle
      | (endAngle `mod'` 360) - startAngle < 180 = RightSemiCircle
      | otherwise = LeftSemiCircle

    halfSize :: Text
    halfSize = tshow $ size / 2

    tripleSize :: Text
    tripleSize = tshow $ size * 1.5

partsToDeg :: Double -> Double
partsToDeg parts = parts * 360
