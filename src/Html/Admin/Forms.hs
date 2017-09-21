{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Html.Admin.Forms where

import Data.Aeson
import Data.Data
import Data.Maybe (isNothing)
import qualified Data.Text as T
import GHC.Generics
import Web.FormUrlEncoded (FromForm, ToForm)

import qualified Models.Author as A
import qualified Models.Post as P

class FromModel a b where
  fromModel :: a -> b

-- Post Things
data PostForm = PostForm
  { seriesId :: Maybe Int
  , title :: !T.Text
  , body :: Maybe T.Text
  , synopsis :: Maybe T.Text
  , publish :: Bool
  , publishDate :: Maybe T.Text
  , ordinal :: Maybe Int
  } deriving (Eq, Show, Generic, Data)

instance FromForm PostForm
instance ToJSON PostForm
instance FromJSON PostForm

instance FromModel P.BlogPost PostForm where
  fromModel post = PostForm {..}
    where
      seriesId = P.seriesId post
      title = P.title post
      body = P.body post
      synopsis = P.synopsis post
      publish = isNothing . P.pubdate $ post
      publishDate = T.pack . show <$> P.pubdate post
      ordinal = P.ordinal post

-- Series Things
data SeriesForm = SeriesForm
  { name :: T.Text
  , description :: T.Text
  , parentId :: Maybe Int
  } deriving (Eq, Show, Generic, Data)

instance FromForm SeriesForm
instance ToJSON SeriesForm
instance FromJSON SeriesForm

instance FromModel P.BlogSeries SeriesForm where
  fromModel series = SeriesForm {..}
    where
      name = P.name series
      description = P.description series
      parentId = P.parentid series

-- Author Things
data AuthorForm = AuthorForm
  { firstName :: !T.Text
  , lastName :: !T.Text
  } deriving (Eq, Show, Generic, Data)

instance FromForm AuthorForm
instance ToJSON AuthorForm
instance FromJSON AuthorForm

instance FromModel A.Author AuthorForm where
  fromModel author = AuthorForm {..}
    where
      firstName = A.firstName author
      lastName = A.lastName author
-- Media Things
