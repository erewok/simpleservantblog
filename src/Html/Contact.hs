{-# LANGUAGE OverloadedStrings #-}

module Html.Contact where

import           Control.Monad.Except
import qualified Data.Text                   as T
import           Servant
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

import           Html.Home                   (PageType (..), pageSkeleton)

data ContactForm = ContactForm
 { cname    :: !T.Text
 , cemail   :: !T.Text
 , cmessage :: !T.Text
 } deriving (Eq, Show)


instance FromFormUrlEncoded ContactForm where
 fromFormUrlEncoded d = do
   name <- case lookup "name" d of
     Nothing -> Left "name field is missing"
     Just  x -> return x
   email <- case lookup "email" d of
     Nothing -> Left "email field is missing"
     Just  x -> return x
   message <- case lookup "message" d of
     Nothing -> Left "message field is missing"
     Just  x -> return x
   return ContactForm
     { cname = name
     , cemail = email
     , cmessage = message }
