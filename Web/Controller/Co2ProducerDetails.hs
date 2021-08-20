module Web.Controller.Co2ProducerDetails where

import Web.Controller.Prelude

import Web.View.Co2ProducerDetails.Index
import Web.View.Co2ProducerDetails.New
import Web.View.Co2ProducerDetails.Edit
import Web.View.Co2ProducerDetails.Show

instance Controller Co2ProducerDetailsController where
    action Co2ProducerDetailsAction = do
        co2ProducerDetails <- query @Co2ProducerDetail |> fetch
        render IndexView { .. }

    action NewCo2ProducerDetailAction { co2ProducerId } = do
        ensureIsUser
        let co2ProducerDetail = newRecord |> set #co2ProducerId co2ProducerId
        co2Producer <- fetch co2ProducerId
        render NewView { .. }

    action ShowCo2ProducerDetailAction { co2ProducerDetailId } = do
        co2ProducerDetail <- fetch co2ProducerDetailId
        render ShowView { .. }

    action EditCo2ProducerDetailAction { co2ProducerDetailId } = do
        ensureIsUser
        co2ProducerDetail <- fetch co2ProducerDetailId
        accessDeniedUnless $ get #userId co2ProducerDetail == currentUserId
        co2Producer <- fetch $ get #co2ProducerId co2ProducerDetail
        render EditView { .. }

    action UpdateCo2ProducerDetailAction { co2ProducerDetailId } = do
        ensureIsUser
        co2ProducerDetail <- fetch co2ProducerDetailId
        accessDeniedUnless $ get #userId co2ProducerDetail == currentUserId
        co2ProducerDetail
            |> buildCo2ProducerDetail
            |> ifValid \case
                Left co2ProducerDetail -> do 
                  co2Producer <- fetch $ get #co2ProducerId co2ProducerDetail
                  render EditView { .. }
                Right co2ProducerDetail -> do
                    co2ProducerDetail <- co2ProducerDetail |> updateRecord
                    setSuccessMessage "Co2ProducerDetail updated"
                    redirectTo ShowCo2ProducerAction { co2ProducerId = get #co2ProducerId co2ProducerDetail }

    action CreateCo2ProducerDetailAction = do
        ensureIsUser
        let co2ProducerDetail = newRecord @Co2ProducerDetail
        co2ProducerDetail
            |> buildCo2ProducerDetail
            |> set #userId currentUserId
            |> ifValid \case
                Left co2ProducerDetail -> do
                  co2Producer <- fetch $ get #co2ProducerId co2ProducerDetail
                  render NewView { .. } 
                Right co2ProducerDetail -> do
                    co2ProducerDetail <- co2ProducerDetail |> createRecord
                    setSuccessMessage "Co2ProducerDetail created"
                    redirectTo ShowCo2ProducerAction { co2ProducerId = get #co2ProducerId co2ProducerDetail }

    action DeleteCo2ProducerDetailAction { co2ProducerDetailId } = do
        ensureIsUser
        co2ProducerDetail <- fetch co2ProducerDetailId
        accessDeniedUnless $ get #userId co2ProducerDetail == currentUserId
        deleteRecord co2ProducerDetail
        setSuccessMessage "Co2ProducerDetail deleted"
        redirectTo ShowCo2ProducerAction { co2ProducerId = get #co2ProducerId co2ProducerDetail }

buildCo2ProducerDetail co2ProducerDetail = 
  co2ProducerDetail
    |> fill @["co2ProducerId","region","year","gCo2e","per","source","userId"]
    |> emptyValueToNothing #region
    |> validateField #gCo2e (isInRange (1, 2000000))
    |> validateField #per (isInRange (1, 2000000))
    |> validateField #source nonEmpty
