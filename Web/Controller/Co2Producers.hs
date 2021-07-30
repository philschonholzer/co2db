module Web.Controller.Co2Producers where

import Web.Controller.Prelude
import Web.View.Co2Producers.Edit
import Web.View.Co2Producers.Index
import Web.View.Co2Producers.New
import Web.View.Co2Producers.Show
import qualified Data.Text as T
import Data.Char (isSpace)

instance Controller Co2ProducersController where
  action Co2ProducersAction = do
    let searchTerm = paramOrNothing @Text "search"
    case searchTerm of
      Nothing -> do
        co2Producers <- query @Co2Producer |> fetch
        render IndexView {..}
      Just "" -> do
        co2Producers <- query @Co2Producer |> fetch
        render IndexView {..}
      Just justSearchTerm -> do
        co2Producers <- query @Co2Producer
          |> filterWhereIMatches (#title, ".*(" <> matchTerm justSearchTerm <> ").*")
          |> fetch
        render IndexView {..}

  action NewCo2ProducerAction = do
    ensureIsUser
    let co2Producer = newRecord
    categories <- query @Category |> fetch
    render NewView {..}

  action ShowCo2ProducerAction {co2ProducerId} = do
    co2Producer <- fetch co2ProducerId
      >>= fetchRelated #co2ProducerDetails
    render ShowView {..}

  action EditCo2ProducerAction {co2ProducerId} = do
    ensureIsUser
    co2Producer <- fetch co2ProducerId
    categories <- query @Category |> fetch
    producerOfUser co2Producer |> accessDeniedUnless
    render EditView {..}

  action UpdateCo2ProducerAction {co2ProducerId} = do
    ensureIsUser
    co2Producer <- fetch co2ProducerId
    producerOfUser co2Producer |> accessDeniedUnless
    co2Producer
      |> buildCo2Producer
      |> ifValid \case
        Left co2Producer -> do
          categories <- query @Category |> fetch
          render EditView {..}
        Right co2Producer -> do
          co2Producer <- co2Producer |> updateRecord
          setSuccessMessage "Co2Producer updated"
          redirectTo ShowCo2ProducerAction {..}

  action CreateCo2ProducerAction = do
    ensureIsUser
    let co2Producer = newRecord @Co2Producer
    co2Producer
      |> buildCo2Producer
      |> set #userId (Just currentUserId)
      |> ifValid \case
        Left co2Producer -> do
          categories <- query @Category |> fetch
          render NewView {..}
        Right co2Producer -> do
          co2Producer <- co2Producer |> createRecord
          setSuccessMessage "Co2Producer created"
          redirectTo Co2ProducersAction

  action DeleteCo2ProducerAction {co2ProducerId} = do
    ensureIsUser
    co2Producer <- fetch co2ProducerId
    producerOfUser co2Producer |> accessDeniedUnless
    deleteRecord co2Producer
    setSuccessMessage "Co2Producer deleted"
    redirectTo Co2ProducersAction



matchTerm :: Text -> Text
matchTerm = filterWhitespace . T.map replaceSeparators
  where
    replaceSeparators :: Char -> Char
    replaceSeparators char | char `elem` [';', ','] = '|'
                           | otherwise  = char

    filterWhitespace :: Text -> Text 
    filterWhitespace = T.filter (not . isSpace)

buildCo2Producer co2Producer =
  co2Producer
    |> fill @["title", "description", "categoryId", "gCo2e", "commonConsumption", "averageYearlyConsumption", "per", "unit", "source", "image"]
    |> validateField #title nonEmpty
    |> validateField #categoryId nonEmpty
    |> validateField #gCo2e (isInRange (1, 2000000))
    |> validateField #per (isInRange (1, 2000000))
    |> validateField #commonConsumption (isInRange (0.0000001, 2000000))
    |> validateField #averageYearlyConsumption (isInRange (0.0000001, 2000000))
    |> validateField #source nonEmpty
    |> emptyValueToNothing #description

producerOfUser co2Producer = get #userId co2Producer |> fromMaybe "empty" == currentUserId