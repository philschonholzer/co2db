module Web.Component.CommonConsumption where

import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple.ToField as PG
import Generated.Types
import IHP.QueryBuilder
import IHP.ServerSideComponent.ControllerFunctions
import IHP.ServerSideComponent.Types as SSC
import IHP.ServerSideComponent.ViewFunctions
import Numeric
import Web.Controller.Prelude hiding (getState, render, setState)
import Web.View.Prelude hiding (fetch, query)

-- The state object
data CommonConsumption = CommonConsumption {value :: !Double, minValue :: !Double, maxValue :: Double, gCo2 :: Double, unit :: Unit}

-- The set of actions
data CommonConsumptionController
  = IncrementCommonConsumptionAction
  | SetCommonConsumptionValue {newValue :: !Double, newGCo2 :: Double}
  deriving (Eq, Show, Data)

$(deriveSSC ''CommonConsumptionController)

instance Component CommonConsumption CommonConsumptionController where
  initialState = CommonConsumption {value = 0, minValue = 0, maxValue = 0, gCo2 = 0, unit = Gram}

  -- The render function
  render CommonConsumption {value, minValue, maxValue, gCo2, unit} =
    [hsx|
        <p style="font-size: 2em;">
          <span class="producer-amount">{value |> twoDec}</span>
          <span class="producer-unit">{unit}</span>
          {svg}
          <span class="co2-amount">{calcCo2Factor gCo2 1.0 value |> renderWeight}</span>&nbsp;CO<sub>2</sub>e
        </p>
        <input data-test={tshow gCo2} class="range" type="range" min={tshow minValue} max={tshow maxValue} step={steps} value={inputValue value} oninput="callServerAction('SetCommonConsumptionValue', { newValue: parseFloat(this.value), newGCo2: parseFloat(this.dataset.test) })" />
    |]
    where
      steps :: String
      steps = showFFloat (Just 2) ((maxValue - minValue) / 50.0) ""

      svg =
        [hsx|
          <svg style="height: 1em; width: 3em;" viewBox="-20 0 140 100">
            <path fill="transparent" stroke="black" stroke-width="10" d="M -10,50 L 90,50 M 50,10 L 90,50 L 50,90"/>
          </svg>
        |]

  -- The action handlers
  action state IncrementCommonConsumptionAction = do
    state
      |> incrementField #value
      |> pure
  action state SetCommonConsumptionValue {newValue, newGCo2} = do
    state
      |> set #value newValue
      |> set #gCo2 newGCo2
      |> pure

instance SetField "value" CommonConsumption Double where setField value' commonConsumption = commonConsumption {value = value'}

instance SetField "gCo2" CommonConsumption Double where setField gCo2' commonConsumption = commonConsumption {gCo2 = gCo2'}
