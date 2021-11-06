module Web.Component.CommonConsumption where

import Data.Fixed
import Generated.Types
import IHP.QueryBuilder
import IHP.ServerSideComponent.ControllerFunctions
import IHP.ServerSideComponent.Types as SSC
import IHP.ServerSideComponent.ViewFunctions
import Numeric
import Text.Printf (printf)
import Web.Controller.Prelude hiding (getState, render, setState)
import Web.View.Prelude hiding (fetch, query)

data CommonConsumption = CommonConsumption
  { amount :: !Double,
    minAmount :: !Double,
    maxAmount :: !Double,
    timesPerYear :: !Double,
    minTimesPerYear :: !Double,
    maxTimesPerYear :: !Double,
    gCo2 :: !Double,
    unit :: !Unit
  }

data CommonConsumptionController = SetValues {newAmount :: !Double, newTimesPerYear :: !Double, newGCo2 :: !Double}
  deriving (Eq, Show, Data)

$(deriveSSC ''CommonConsumptionController)

instance Component CommonConsumption CommonConsumptionController where
  initialState =
    CommonConsumption
      { amount = 0,
        minAmount = 0,
        maxAmount = 0,
        timesPerYear = 0,
        minTimesPerYear = 0,
        maxTimesPerYear = 0,
        gCo2 = 0,
        unit = Gram
      }

  render CommonConsumption {amount, minAmount, maxAmount, timesPerYear, minTimesPerYear, maxTimesPerYear, gCo2, unit} =
    [hsx|
        <p style="font-size: 2em;">
          <span class="producer-amount">{amount |> twoDec}</span>
          <span class="producer-unit">&ensp;{unit}</span>
          {arrow}
          <span class="co2-amount amount">{calcCo2PerConsumption gCo2 1.0 amount |> renderWeight}</span>&nbsp;CO<sub>2</sub>e / consumption
        </p>
        
        <p style="font-size: 2em;">
          <span style="font-size: 1.5em">×&ensp;</span>
          <span class="producer-amount">{timesPerYear |> twoDec}</span> / year
        </p>
        <p style="font-size: 2em;">
          <span style="font-size: 1.5em">=&ensp;</span>
          <span class="co2-amount timesPerYear">{calcCo2PerConsumption gCo2 1.0 amount |> calcCo2PerYear timesPerYear |> renderWeight}</span>&nbsp;CO<sub>2</sub>e / year
        </p>

        {renderCharts $ calcCo2PerConsumption gCo2 1.0 amount |> calcCo2PerYear timesPerYear |> partsOfOnePersonFootPrintPerYear}

        {renderInput amount minAmount maxAmount "amountInput" "Single consumption"}
        {renderInput timesPerYear minTimesPerYear maxTimesPerYear "timesPerYearInput" "Times per year"}

        {onInputScript}
      |]
    where
      renderInput :: Double -> Double -> Double -> String -> String -> Html
      renderInput value minValue maxValue inputId labelTitle = case (minValue, maxValue) of
        (min, max) | min == max -> [hsx||]
        _ ->
          [hsx|
            <label for={inputId}>{labelTitle}</label>
            <input 
              id={inputId}
              class="range" 
              type="range" 
              min={tshow minValue} 
              max={tshow maxValue} 
              step={steps} 
              value={inputValue value} 
              oninput="onInput()" 
            />
          |]
          where
            steps :: String
            steps = showFFloat (Just 2) ((maxValue - minValue) / 50.0) ""

      onInputScript =
        [hsx|
          <script id="consumptionScript" data-gco2={tshow gCo2}>
            function onInput () {
              callServerAction('SetValues', 
                { 
                  newAmount: parseFloat(amountInput.value), 
                  newTimesPerYear: parseFloat(timesPerYearInput.value), 
                  newGCo2: parseFloat(consumptionScript.dataset.gco2) 
                }
              )
            }
          </script>
        |]

      arrow =
        [hsx|
          <svg style="height: 1em; width: 3em;" viewBox="-20 0 140 100">
            <path fill="transparent" stroke="black" stroke-width="10" d="M -10,50 L 90,50 M 50,10 L 90,50 L 50,90"/>
          </svg>
        |]

      renderCharts :: Double -> Html
      renderCharts totalAmount =
        [hsx|
          <div>
            <div style="display: flex; justify-content: space-around; padding-left: 100px; padding-right: 100px;">
              <svg style="height: 0; position: absolute; width: 0;">
                {segmentMask (SvgPoint 150 100) 300 0 $ (totalAmount |> (`mod'` 1) |> partsToDeg )}
              </svg>
              {forEach (totalAmount |> fromPartsToListOfCharts) renderPieChart}
            </div>
            <div style="text-align: center; font-size: 2em;"><b>{totalAmount |> (*100) |> twoDec |> (++ "%")}</b><br/>of the complet ø CO<sub>2</sub>e Footprint of 1<svg viewBox="0 -5 20 60" style="height: 1em; width: 1em; transform: translateY(1px);">{personShape}</svg>per year</div>
          </div>
        |]
        where
          renderPieChart :: Double -> Html
          renderPieChart amount =
            [hsx|
              <div style="margin-left: -140px; margin-right: -140px; transition: flex 1s;" >
                <svg style="width: 100%;" viewBox="0 0 300 200">
                  <style>
                    .heavy { font: bold  20px sans-serif; }
                    .shadow {filter: drop-shadow( 0px 0px 8px rgba(0, 0, 0, 0.2));}
                  </style>
                  <circle cx="150" cy="100" r="75" fill="lightgrey" class="shadow" />
                  <circle cx="150" cy="100" r="75" fill="red" mask={if amount /= 1.0 then "url(#segmentMask)" else tshow ""}/>
                  <g transform="translate(95,80) scale(0.8)">{personShape}</g>
                </svg>
              </div>
            |]

          personShape :: Html
          personShape =
            [hsx|
              <circle cx="9" cy="5" r="4"/><path d="M9,34.601l0.663,0l0,17.704l4.237,0l0,-36.252l0.893,-0l-0,13.846l2.886,0l-0,-18.048l-17.358,0l0,18.048l2.886,0l0,-13.846l0.893,-0l-0,36.252l4.237,0l-0,-17.704l0.663,0"/>
            |]

      fromPartsToListOfCharts :: Double -> [Double]
      fromPartsToListOfCharts parts = replicate (floor parts) 1 <> [parts `mod'` 1]

  -- The action handlers
  action state SetValues {newAmount, newTimesPerYear, newGCo2} = do
    state
      |> set #amount newAmount
      |> set #timesPerYear newTimesPerYear
      |> set #gCo2 newGCo2
      |> pure

instance SetField "amount" CommonConsumption Double where setField amount' commonConsumption = commonConsumption {amount = amount'}

instance SetField "timesPerYear" CommonConsumption Double where setField timesPerYear' commonConsumption = commonConsumption {timesPerYear = timesPerYear'}

instance SetField "gCo2" CommonConsumption Double where setField gCo2' commonConsumption = commonConsumption {gCo2 = gCo2'}
