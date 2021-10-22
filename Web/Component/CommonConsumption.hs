module Web.Component.CommonConsumption where

import Generated.Types
import IHP.QueryBuilder
import IHP.ServerSideComponent.ControllerFunctions
import IHP.ServerSideComponent.Types as SSC
import IHP.ServerSideComponent.ViewFunctions
import Numeric
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
          {svg}
          <span class="co2-amount amount">{calcCo2Factor gCo2 1.0 amount |> renderWeight}</span>&nbsp;CO<sub>2</sub>e / consumption
        </p>
        
        <p style="font-size: 2em;">
          <span style="font-size: 1.5em">×&ensp;</span>
          <span class="producer-amount">{timesPerYear |> twoDec}</span> / year
        </p>
        <p style="font-size: 2em;">
          <span style="font-size: 1.5em">=&ensp;</span>
          <span class="co2-amount timesPerYear">{((calcCo2Factor gCo2 1.0 amount) * timesPerYear) |> renderWeight}</span>&nbsp;CO<sub>2</sub>e / year
        </p>

        <svg style="width: 100%;" viewBox="0 0 300 200">
          <style>
            .heavy { font: bold  20px sans-serif; }
          </style>
          {segmentMask (SvgPoint 150 100) 300 0 $ ((calcCo2Factor gCo2 1.0 amount) * timesPerYear) / 20000}
          <circle cx="150" cy="100" r="75" fill="lightgrey" />
          <circle cx="150" cy="100" r="75" fill="red" mask="url(#segmentMask)"/>
          <text x="100" y="107" class="heavy"  >{((calcCo2Factor gCo2 1.0 amount) * timesPerYear)}</text>
        </svg>

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

      svg =
        [hsx|
          <svg style="height: 1em; width: 3em;" viewBox="-20 0 140 100">
            <path fill="transparent" stroke="black" stroke-width="10" d="M -10,50 L 90,50 M 50,10 L 90,50 L 50,90"/>
          </svg>
        |]

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
