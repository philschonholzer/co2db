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
    setTitle "CO₂ emissions"
    case searchTerm of
      Nothing -> do
        co2Producers <- query @Co2Producer |> fetch >>= collectionFetchRelated #categoryId >>= collectionFetchRelated #sources
        render IndexView {..}
      Just "" -> do
        co2Producers <- query @Co2Producer |> fetch >>= collectionFetchRelated #categoryId >>= collectionFetchRelated #sources
        render IndexView {..}
      Just justSearchTerm -> do
        co2Producers <- query @Co2Producer
          |> filterWhereIMatches (#title, ".*(" <> matchTerm justSearchTerm <> ").*")
          |> fetch >>= collectionFetchRelated #categoryId >>= collectionFetchRelated #sources
        render IndexView {..}

  action NewCo2ProducerAction = do
    ensureIsUser
    let co2Producer = newRecord
    categories <- query @Category |> fetch
    render NewView {..}

  action ShowCo2ProducerAction {co2ProducerId, slug} = do
    co2Producer <- fetchCo2Producer co2ProducerId slug >>= fetchRelated #sources
    setTitle $ get #title co2Producer <> " CO₂ emission"
    render ShowView {..}

  action EditCo2ProducerAction {co2ProducerId, slug} = do
    ensureIsUser
    co2Producer <- fetchCo2Producer co2ProducerId slug
    categories <- query @Category |> fetch
    accessDeniedUnless $ get #userId co2Producer == currentUserId
    setTitle $ "Edit " <> get #title co2Producer <> " CO₂ emission"
    render EditView {..}

  action UpdateCo2ProducerAction {co2ProducerId, slug} = do
    ensureIsUser
    co2Producer <- fetchCo2Producer co2ProducerId slug
    accessDeniedUnless $ get #userId co2Producer == currentUserId
    co2Producer
      |> buildCo2Producer
      |> withCustomErrorMessageIO "A CO₂ producer with this slug exists already. (The slug is generated from the title)" validateIsUnique #slug
      >>= ifValid \case
        Left co2Producer -> do
          categories <- query @Category |> fetch
          setErrorMessage "Could not create CO₂ producer. See if all fields have valid values."
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
      |> withCustomErrorMessageIO "A CO₂ producer with this slug exists already. (The slug is generated from the title)" validateIsUnique #slug
      >>= ifValid \case
        Left co2Producer -> do
          categories <- query @Category |> fetch
          setErrorMessage "Could not create CO₂ producer. See if all fields have valid values."
          render NewView {..}
        Right co2Producer -> do
          co2Producer <- co2Producer |> createRecord
          redirectTo NewSourceAction { co2ProducerId = get #id co2Producer}

  action DeleteCo2ProducerAction {co2ProducerId, slug} = do
    ensureIsUser
    co2Producer <- fetchCo2Producer co2ProducerId slug
    accessDeniedUnless $ get #userId co2Producer == currentUserId
    deleteRecord co2Producer
    setSuccessMessage "Co2Producer deleted"
    redirectTo Co2ProducersAction

fetchCo2Producer id slug = case slug of
            Just slug -> query @Co2Producer |> filterWhere (#slug, slug) |> fetchOne
            Nothing   -> fetchOne id

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
    |> fill @["title", "description", "slug", "categoryId", "image", "unit", "singleConsumptionFrom", "singleConsumptionTo", "singleConsumptionAverage", "timesPerYearFrom", "timesPerYearTo", "timesPerYearAverage"]
    |> validateField #title nonEmpty
    |> validateField #categoryId nonEmpty
    |> emptyValueToNothing #description
    |> (slugifyField #slug =<< get #title)
    |> validateField #singleConsumptionFrom (isGreaterEqualThan 0)
    |> (validateField #singleConsumptionTo =<< isGreaterEqualThan . get #singleConsumptionFrom) -- same as (flip (validateField #singleConsumptionTo) <*> (isGreaterEqualThan . get #singleConsumptionFrom))
    |> (validateField #singleConsumptionAverage =<< isInRange . ofFields #singleConsumptionFrom #singleConsumptionTo)
    |> validateField #timesPerYearFrom (isGreaterEqualThan 0)
    |> (validateField #timesPerYearTo =<< isGreaterEqualThan . get #timesPerYearFrom)
    |> (validateField #timesPerYearAverage =<< isInRange . ofFields #timesPerYearFrom #timesPerYearTo)
  where
    ofFields fieldFrom fieldTo = (,) <$> get fieldFrom <*> get fieldTo

isGreaterEqualThan :: (Show value, Ord value) => value -> value -> ValidatorResult
isGreaterEqualThan min value | value >= min = Success
isGreaterEqualThan min value = Failure ("has to be greater or equal than " <> tshow min)


slugifyField field value = modify field (\_ -> toSlug value)