module Web.Controller.Sources where

import Web.Controller.Prelude

import Web.View.Sources.Index
import Web.View.Sources.New
import Web.View.Sources.Edit
import Web.View.Sources.Show

instance Controller SourcesController where
    action SourcesAction = do
        sources <- query @Source |> fetch
        render IndexView { .. }

    action NewSourceAction { co2ProducerId } = do
        ensureIsUser
        let source = newRecord |> set #co2ProducerId co2ProducerId
        co2Producer <- fetch co2ProducerId
        setTitle $ "New Source for " <> get #title co2Producer
        render NewView { .. }

    action ShowSourceAction { sourceId } = do
        source <- fetch sourceId
        render ShowView { .. }

    action EditSourceAction { sourceId } = do
        ensureIsUser
        source <- fetch sourceId
        accessDeniedUnless $ get #userId source == currentUserId
        co2Producer <- fetch $ get #co2ProducerId source
        setTitle $ "Edit a Source for " <> get #title co2Producer
        render EditView { .. }

    action UpdateSourceAction { sourceId } = do
        ensureIsUser
        source <- fetch sourceId
        accessDeniedUnless $ get #userId source == currentUserId
        source
            |> buildSource
            |> ifValid \case
                Left source -> do 
                  co2Producer <- fetch $ get #co2ProducerId source
                  render EditView { .. }
                Right source -> do
                    source <- source |> updateRecord
                    setSuccessMessage "Source updated"
                    redirectTo ShowCo2ProducerAction { co2ProducerId = get #co2ProducerId source }

    action CreateSourceAction = do
        ensureIsUser
        let source = newRecord @Source
        source
            |> buildSource
            |> set #userId currentUserId
            |> ifValid \case
                Left source -> do
                  co2Producer <- fetch $ get #co2ProducerId source
                  render NewView { .. } 
                Right source -> do
                    source <- source |> createRecord
                    setSuccessMessage "Source created"
                    redirectTo ShowCo2ProducerAction { co2ProducerId = get #co2ProducerId source }

    action DeleteSourceAction { sourceId } = do
        ensureIsUser
        source <- fetch sourceId
        accessDeniedUnless $ get #userId source == currentUserId
        deleteRecord source
        setSuccessMessage "Source deleted"
        redirectTo ShowCo2ProducerAction { co2ProducerId = get #co2ProducerId source }

buildSource source = 
  source
    |> fill @["co2ProducerId","region","year","gCo2e","per","description","userId"]
    |> emptyValueToNothing #region
    |> validateField #gCo2e (isInRange (1, 2000000))
    |> validateField #per (isInRange (1, 2000000))
    |> validateField #description nonEmpty
