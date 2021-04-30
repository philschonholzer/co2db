module Web.Controller.Co2Emitters where

import Web.Controller.Prelude
import Web.View.Co2Emitters.Edit
import Web.View.Co2Emitters.Index
import Web.View.Co2Emitters.New
import Web.View.Co2Emitters.Show

instance Controller Co2EmittersController where
  action Co2EmittersAction = do
    co2Emitters <- query @Co2Emitter |> fetch
    render IndexView {..}
  action NewCo2EmitterAction = do
    let co2Emitter = newRecord
    categories <- query @Category |> fetch
    render NewView {..}
  action ShowCo2EmitterAction {co2EmitterId} = do
    co2Emitter <- fetch co2EmitterId
    render ShowView {..}
  action EditCo2EmitterAction {co2EmitterId} = do
    co2Emitter <- fetch co2EmitterId
    categories <- query @Category |> fetch
    render EditView {..}
  action UpdateCo2EmitterAction {co2EmitterId} = do
    co2Emitter <- fetch co2EmitterId
    co2Emitter
      |> buildCo2Emitter
      |> ifValid \case
        Left co2Emitter -> do
          categories <- query @Category |> fetch
          render EditView {..}
        Right co2Emitter -> do
          co2Emitter <- co2Emitter |> updateRecord
          setSuccessMessage "Co2Emitter updated"
          redirectTo EditCo2EmitterAction {..} 
  action CreateCo2EmitterAction = do
    let co2Emitter = newRecord @Co2Emitter
    co2Emitter
      |> buildCo2Emitter
      |> ifValid \case
        Left co2Emitter -> do
          categories <- query @Category |> fetch
          render NewView {..}
        Right co2Emitter -> do
          co2Emitter <- co2Emitter |> createRecord
          setSuccessMessage "Co2Emitter created"
          redirectTo Co2EmittersAction
  action DeleteCo2EmitterAction {co2EmitterId} = do
    co2Emitter <- fetch co2EmitterId
    deleteRecord co2Emitter
    setSuccessMessage "Co2Emitter deleted"
    redirectTo Co2EmittersAction

buildCo2Emitter co2Emitter =
  co2Emitter
    |> fill @["title", "description", "categoryId", "gCo2e", "per", "unit", "source", "image"]
    |> validateField #title nonEmpty
    |> validateField #categoryId nonEmpty
    |> validateField #gCo2e (isInRange (0, 2000000))
    |> validateField #per (isInRange (0, 2000000))
    |> validateField #source nonEmpty
    |> emptyValueToNothing #description
