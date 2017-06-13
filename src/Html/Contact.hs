{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Html.Contact where

import           Control.Exception           (throwIO)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import           GHC.Generics
import           Network.Mail.Client.Gmail   (sendGmail)
import           Network.Mail.Mime           (Address(..))
import           Servant
import           Servant.HTML.Blaze
import           System.Environment          (lookupEnv)
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A
import           Web.FormUrlEncoded          (FromForm, Form)

import           Html.Home                   (PageType (..), contactForm,
                                              homeSkeleton, redirectPage)

data ContactForm = ContactForm
 { cname    :: !T.Text
 , cemail   :: !T.Text
 , cmessage :: !T.Text
 } deriving (Eq, Show, Generic)

instance FromForm ContactForm

data EmailCreds = EmailCreds
  { emailUsername :: !T.Text
  , emailPasswd   :: !T.Text
  , emailHost     :: !T.Text
} deriving (Eq, Show)

type ContactApi = "contact" :> ReqBody '[FormUrlEncoded] ContactForm
                                   :> Post '[HTML] Html
                :<|> "contact" :> Get '[HTML] Html
                :<|> "thanks" :> Get '[HTML] Html

contactServer :: Server ContactApi
contactServer = contactPostH :<|> contactGetH :<|> thanksH
  where contactGetH = return $ contactPage True
        thanksH = pure thanksPage

contactPage  :: Bool -> H.Html
contactPage firstTime = docTypeHtml $ homeSkeleton $ NoJS $
      H.div ! A.class_ "row" $
        H.div ! A.id "contact-page" $
          H.div ! A.class_ "contact-page-box" $ do
            let formOrError = if firstTime then contactForm else H.p "Please fill in all fields"
            formOrError

contactPostH :: ContactForm -> Handler Html
contactPostH contactF = do
  liftIO $ sendContact contactF
  pure $ redirectPage "/thanks"

thanksPage :: H.Html
thanksPage = docTypeHtml $ homeSkeleton $ NoJS $
      H.div ! A.class_ "row" $
        H.div ! A.id "thanks-page" ! A.class_ "container" $
          H.div ! A.class_ "contact-page-box" $
            H.div ! A.class_ "u-full-width" $ do
              H.h2 "Thanks"
              H.p "We'll get back to you soon."

constructSubject :: ContactForm -> T.Text
constructSubject form = "New message from " `mappend` cname form

constructBody :: ContactForm -> T.Text
constructBody form = T.unlines ["From: " `mappend` cname form
                             , "\nEmail: " `mappend` cemail form
                             ,  "\n\n"
                             , cmessage form]

sendContact :: ContactForm -> IO ()
sendContact contact = do
  creds <- getEmailCreds
  case creds of
    Nothing -> throwIO (userError "Email credentials not present in environment")
    Just creds -> do
      let from = addressTrunk $ emailUsername creds
          to = [addressTrunk $ emailUsername creds]
          cc = []
          bcc = []
          attachments = []
          timeout = 6000000
          subject = constructSubject contact
          body = constructBody contact
      sendGmail
        (L.fromStrict $ emailUsername creds)
          (L.fromStrict $ emailPasswd creds)
            from
              to cc bcc
                subject (L.fromStrict body) attachments timeout
    where addressTrunk = Address (Just "Erik Aker")

getEmailCreds :: IO (Maybe EmailCreds)
getEmailCreds = do
 emailUser <- lookupEnv "EMAIL_USER"
 emailPasswd <- lookupEnv "EMAIL_PASSWD"
 emailHost <- lookupEnv "EMAIL_HOST"
 pure $ EmailCreds <$>
   (T.pack <$> emailUser)
     <*> (T.pack <$> emailPasswd)
       <*> (T.pack <$> emailHost)
