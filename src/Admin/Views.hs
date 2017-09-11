{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}


module Admin.Views where

import           Data.Data
import           Data.Monoid                             ((<>))
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Data.Time                               (UTCTime)
import           Servant.HTML.Blaze
import           Text.Blaze.Html5                        as H
import           Text.Blaze.Html5.Attributes             as A

import qualified Models.Author                           as A
import qualified Models.Post                             as P
-- Unimplemented yet:
import qualified Models.Media                            as M
import qualified Models.Projects                         as Proj



-- These functions determine the actual HTML you will get for rendering one of these things
asListModels :: (H.ToMarkup a) => [a] -> Html
asListModels values = H.ul $ mconcat $ fmap (H.li . H.toMarkup) values

asTableModels :: (Data a, H.ToMarkup b) => a -> [[b]] -> Html
asTableModels _ [] = ""
asTableModels model values = H.table $ do
  tableHeader model
  H.tbody $
    viewTableRowsModels values

tableHeader :: forall a. Data a => a -> Html
tableHeader model = H.thead $
    H.tr $
      mconcat $ fmap (H.th . H.toMarkup) (getFieldsForInstance model)

viewTableRowsModels :: (H.ToMarkup a) => [[a]] -> Html
viewTableRowsModels values = mconcat $ fmap modelToRow values
  where modelToRow values' = H.tr $ mconcat $ fmap (H.td . H.toMarkup) values'

editRender :: (Text, ViewModelField ModelField) -> Html
editRender (fieldname, ReadOnly fl) = parentDiv fieldname $ H.toMarkup $ ReadOnly fl
editRender (fieldname, InlineEditable fl) = parentDiv fieldname $ toEditField fieldname fl
editRender (fieldname, Truncated fl) = parentDiv fieldname $ toEditField fieldname fl
editRender (fieldname, Editable fl) = parentDiv fieldname $ toEditField fieldname fl

parentDiv :: Text -> Html -> Html
parentDiv fieldname = H.div ! A.id (H.toValue fieldname <> "-parent")

-- Made this thing so it's easier to know what's getting passed where
data FieldNameType = FieldNameType { fldname :: Text, fldtype :: Text}

genericEditField :: (ToValue a) => FieldNameType -> a -> Html
genericEditField fieldinfo val = do
  H.label ! A.for nameVal $ H.text name'
  H.input ! A.id  nameVal ! A.type_ infoVal ! A.value (H.toValue val)
  where name' = fldname fieldinfo
        nameVal = H.toValue name'
        infoVal = H.toValue (fldtype fieldinfo)

-- Instances required to collect all behavior into some simple functions
instance AdminRender P.BlogPost where

  toTableView [] = ""
  toTableView allPosts@(po:_) = asTableModels po $ fmap postMarkup allPosts

  toListView [] = ""
  toListView values = asListModels $ mconcat $ fmap postMarkup values

  toEditSingleView post = mconcat $ fmap editRender (zip (getFieldsForInstance post) (postMarkup post))


instance AdminRender P.BlogSeries where

  toTableView [] = ""
  toTableView allSeries@(ser:_) = asTableModels ser $ fmap seriesMarkup allSeries

  toListView [] = ""
  toListView values = asListModels $ mconcat $ fmap seriesMarkup values

  toEditSingleView series = mconcat $ fmap editRender (zip (getFieldsForInstance series) (seriesMarkup series))


instance AdminRender A.Author where

  toTableView [] = ""
  toTableView allAuthors@(auth:_) = asTableModels auth $ fmap authorMarkup allAuthors

  toListView [] = ""
  toListView values = asListModels $ mconcat $ fmap authorMarkup values

  toEditSingleView author = mconcat $ fmap editRender (zip (getFieldsForInstance author) (authorMarkup author))


instance AdminRender M.Media where

  toTableView [] = ""
  toTableView allMedia@(med:_) = asTableModels med $ fmap mediaMarkup allMedia

  toListView [] = ""
  toListView values = asListModels $ mconcat $ fmap mediaMarkup values

  toEditSingleView media' = mconcat $ fmap editRender (zip (getFieldsForInstance media') (mediaMarkup media'))


-- Some boilerplaty stuff
toEditField :: Text -> ModelField -> Html
toEditField fieldname (N n) = genericEditField (FieldNameType fieldname "number") n
toEditField fieldname (MN Nothing) = genericEditField (FieldNameType fieldname "number") (0 :: Int)
toEditField fieldname (MN (Just n)) = genericEditField (FieldNameType fieldname "number") n
toEditField fieldname (W t) = genericEditField (FieldNameType fieldname "text") t
toEditField fieldname (MW Nothing) = genericEditField (FieldNameType fieldname "text") ("" :: Text)
toEditField fieldname (MW (Just t)) = genericEditField (FieldNameType fieldname "text") t
toEditField fieldname (C tm) = genericEditField (FieldNameType fieldname "date") $ show tm
toEditField fieldname (MC Nothing) = genericEditField (FieldNameType fieldname "date") ("" :: Text)
toEditField fieldname (MC (Just tm)) = genericEditField (FieldNameType fieldname "date") $ show tm
toEditField fieldname (B b') = genericEditField (FieldNameType fieldname "checkbox") $ show b'
toEditField fieldname (MB Nothing) = genericEditField (FieldNameType fieldname "checkbox") ("false" :: Text)
toEditField fieldname (MB (Just b')) = genericEditField (FieldNameType fieldname "checkbox") $ show b'


-- The Markup functions themselves make things renderable
postMarkup :: P.BlogPost -> [ViewModelField ModelField]
postMarkup post = [ReadOnly (N $ P.bid post)
                 , ReadOnly (N $ P.authorId post)
                 , ReadOnly (MN $ P.seriesId post)
                 , InlineEditable (W $ P.title post)
                 , Truncated (MW $ P.body post)
                 , Truncated (MW $ P.synopsis post)
                 , ReadOnly (C $ P.created post)
                 , ReadOnly (MC $ P.modified post)
                 , InlineEditable (MC $ P.pubdate post)
                 , InlineEditable (MN $ P.ordinal post)]

seriesMarkup :: P.BlogSeries -> [ViewModelField ModelField]
seriesMarkup series = [ReadOnly (N $ P.sid series)
                      , InlineEditable (W $ P.name series)
                      , Truncated (W $ P.description series)
                      , ReadOnly (MN $ P.parentid series)]


authorMarkup :: A.Author -> [ViewModelField ModelField]
authorMarkup author = [ReadOnly (N $ A.aid author)
                      , ReadOnly (N $ A.userid author)
                      , InlineEditable (W $ A.firstName author)
                      , InlineEditable (W $ A.lastName author)]


mediaMarkup :: M.Media -> [ViewModelField ModelField]
mediaMarkup media' = [InlineEditable (W $ M.name media')
                     , ReadOnly (W $ M.url media')
                     , ReadOnly (W $ M.location media')
                     , Editable (W $ M.description media')]

-- Class, Instance, and other helper definitions to marry the things together
data ViewModelField a = ReadOnly a
                      | Truncated a
                      | InlineEditable a
                      | Editable a
                      deriving (Eq, Show, Functor)

class AdminRender a where

  toListView :: [a] -> Html
  toTableView :: [a] -> Html
  toEditSingleView :: a -> Html


instance (Show a) => H.ToMarkup (ViewModelField a) where
  toMarkup (ReadOnly fl) = H.toMarkup $ show fl
  toMarkup (Truncated fl) = H.toMarkup $ slice 0 truncationLength $ show fl
  toMarkup (InlineEditable fl) = H.span ! A.class_ "inline-edit-field" $ H.toMarkup $ show fl
  toMarkup (Editable fl) = H.span ! A.class_ "edit-field" $ H.toMarkup $ show fl

instance Show ModelField where
  show (N n) = show n
  show (MN Nothing) = ""
  show (MN (Just n)) = show n
  show (W t) = show t
  show (MW Nothing) = ""
  show (MW (Just t)) = show t
  show (C tm) = show tm
  show (MC Nothing) = ""
  show (MC (Just tm)) = show tm
  show (B b') = show b'
  show (MB Nothing) = ""
  show (MB (Just b')) = show b'


getFieldsForInstance :: forall a. Data a => a -> [Text]
getFieldsForInstance = fmap T.pack . mconcat . fmap constrFields . getConstr
  where getConstr = dataTypeConstrs . dataTypeOf

truncationLength :: Int
truncationLength = 50

slice :: Int -> Int -> [a] -> [a]
slice start' end = take (end - start' + 1) . drop start'


data ModelField = N Int
                | MN (Maybe Int)
                | W Text
                | MW (Maybe Text)
                | C UTCTime
                | MC (Maybe UTCTime)
                | B Bool
                | MB (Maybe Bool)
                deriving (Eq)
