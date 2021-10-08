module Web.Controller.Co2Producers where

import Web.Controller.Prelude
import Web.View.Co2Producers.Edit
import Web.View.Co2Producers.Index
import Web.View.Co2Producers.New
import Web.View.Co2Producers.Show
import qualified Control.Category as C
import qualified Data.Text as T
import Data.Char (isSpace)

instance Controller Co2ProducersController where
  action Co2ProducersAction = do
    let searchTerm = paramOrNothing @Text "search"
    setTitle "CO₂ Footprints"
    case searchTerm of
      Nothing -> do
        co2Producers <- query @Co2Producer |> fetch >>= collectionFetchRelated #sources
        render IndexView {..}
      Just "" -> do
        co2Producers <- query @Co2Producer |> fetch >>= collectionFetchRelated #sources
        render IndexView {..}
      Just justSearchTerm -> do
        co2Producers <- query @Co2Producer
          |> filterWhereIMatches (#title, ".*(" <> matchTerm justSearchTerm <> ").*")
          |> fetch >>= collectionFetchRelated #sources
        render IndexView {..}

  action NewCo2ProducerAction = do
    ensureIsUser
    let co2Producer = newRecord
    categories <- query @Category |> fetch
    render NewView {..}

  action ShowCo2ProducerAction {co2ProducerId} = do
    co2Producer <- fetch co2ProducerId
      >>= fetchRelated #sources
    setTitle $ get #title co2Producer <> " CO₂ Footprint"
    render ShowView {..}

  action EditCo2ProducerAction {co2ProducerId} = do
    ensureIsUser
    co2Producer <- fetch co2ProducerId
    categories <- query @Category |> fetch
    accessDeniedUnless $ get #userId co2Producer == currentUserId
    setTitle $ "Edit " <> get #title co2Producer <> " CO₂ Footprint"
    render EditView {..}

  action UpdateCo2ProducerAction {co2ProducerId} = do
    ensureIsUser
    co2Producer <- fetch co2ProducerId
    accessDeniedUnless $ get #userId co2Producer == currentUserId
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
      |> set #userId currentUserId
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
    accessDeniedUnless $ get #userId co2Producer == currentUserId
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
    |> fill @["title", "description", "categoryId", "image", "unit", "singleConsumptionFrom", "singleConsumptionTo", "singleConsumptionAverage", "timesPerYearFrom", "timesPerYearTo", "timesPerYearAverage"]
    |> validateField #title nonEmpty
    |> validateField #categoryId nonEmpty
    |> emptyValueToNothing #description
    |> validateField #singleConsumptionFrom (isGreaterEqualThan 0)
    |> (validateField #singleConsumptionTo <$> (isGreaterEqualThan . get #singleConsumptionFrom) <*> C.id)
    |> (validateField #singleConsumptionAverage <$> (isInRange . ofFields #singleConsumptionFrom #singleConsumptionTo) <*> C.id)
    |> validateField #timesPerYearFrom (isGreaterEqualThan 0)
    |> (validateField #timesPerYearTo <$> (isGreaterEqualThan . get #timesPerYearFrom) <*> C.id)
    |> (validateField #timesPerYearAverage <$> (isInRange . ofFields #timesPerYearFrom #timesPerYearTo) <*> C.id)
  where
    ofFields fieldFrom fieldTo = (,) <$> get fieldFrom <*> get fieldTo

isGreaterEqualThan :: (Show value, Ord value) => value -> value -> ValidatorResult
isGreaterEqualThan min value | value >= min = Success
isGreaterEqualThan min value = Failure ("has to be greater or equal than " <> tshow min)
