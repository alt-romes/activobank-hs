{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
   Functions for accessing ActivoBank directly
 -}
module ActivoBank
  (
    -- * ActivoBank

    -- ** Session
    withSession

    -- ** Movements
  , fetchMovementsTable
  , Movement(..)
  ) where

import GHC.Conc

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Data.Vector as V

import Text.HTML.Scalpel.Core

import Control.Monad.IO.Class (liftIO)
import Control.Exception (throwIO)

import Data.Char
import Data.Proxy

import Data.Aeson.Types
import Web.FormUrlEncoded

import Network.HTTP.Client (createCookieJar)
import Network.HTTP.Client.TLS

import Servant.API
import qualified Servant.Client as C


-- * Main

-- | A 'Movement' describes a transaction with ActivoBank
data Movement = Mov { movDay  :: Day    -- ^ Movement day
                    , movDesc :: String -- ^ Description
                    , movAmt  :: String -- ^ Amount
                    , movBal  :: String -- ^ Balance
                    }
                deriving Show

-- | Run a 'C.ClientM' within an ActivoBank session.
--
-- Takes the access codes and the user code, starts a session, and runs the
-- provided client within that session. Note that attempts to run the client
-- without the session will fail.
--
-- === Example
--
-- @
-- withSession [1,2,3,4,5,6,7] \"ABCDEF12345\" 'fetchMovementsTable'
-- @
withSession :: [Int] -> String -> C.ClientM a -> IO a
withSession codes user clientF = do

  m <- newTlsManager
  kj <- Just <$> newTVarIO (createCookieJar []) -- cookies are only propagated if we start with a cookie jar

  C.runClientM (createSession >> clientF) (C.ClientEnv m activoBankHost kj C.defaultMakeClientRequest) >>= \case
    Left e -> throwIO e
    Right mt -> pure mt

  where
    -- hardcoded
    fingerprint = "1da5bd6bd9d3fb843bbd4785a4f7d691"
    browserI    = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:104.0) Gecko/20100101 Firefox/104.0|;Portable Document Format;Portable Document Format;Portable Document Format;Portable Document Format;Portable Document Format|1440x900|30|||||MacIntel|en-US|Central European Standard Time"
    referer     = "https://ind.activobank.pt/_loginV2/BlueMainLoginCdm.aspx?ReturnUrl=https%3a%2f%2find.activobank.pt%2fpt%2fprivate%2fdia-a-dia%2fPages%2fdia-a-dia.aspx%3fLanguage%3dpt"

    createSession :: C.ClientM ()
    createSession = do
      -- Get the required access tokens
      AccessCodeDigits a b c <- validateUser (WithInfo fingerprint browserI (SimpleUser user))

      let access = AccessCodeDigits (codes !! (a-1)) (codes !! (b-1)) (codes !! (c-1))

      -- Confirmation will set the user cookie for the next request
      _ <- confirmValidation referer (WithInfo fingerprint browserI access)

      pure ()


-- | Fetch the list of movements from ActivoBank in the last 5 days.
--
-- This function should be run with a session (using 'withSession')
fetchMovementsTable :: C.ClientM [Movement]
fetchMovementsTable = do

    -- Get today
    today <- utctDay <$> liftIO getCurrentTime

    -- Get movements using set cookies
    getMovements (MovementsRequest (addDays (-5) today) today)


-- * ActivoBank API


activoBankHost :: C.BaseUrl
activoBankHost = C.BaseUrl C.Https "ind.activobank.pt" 443 ""

type AB
  =    "_loginV2" :> "BlueMainLoginPageCdmV2.aspx" :> "ValidateUser" :> ReqBody '[JSON] (WithInfo SimpleUser) :> Post '[JSON] AccessCodeDigits

  :<|> "_loginV2" :> "BlueMainLoginPageCdmV2.aspx" :> "ComfirmValidation" :> Header' '[Required, Strict] "Referer" String :> ReqBody '[JSON] (WithInfo AccessCodeDigits) :> Post '[JSON] Value

  :<|> "WebPages" :> "UIServices" :> "ContentCall.aspx" :> ReqBody '[FormUrlEncoded] MovementsRequest :> Post '[HTML] [Movement]

-- | Must be called within the same 'runClientM' as the login comprising of
-- @validateUser >> confirmValidation@
getMovements :: MovementsRequest -> C.ClientM [Movement]
validateUser :: WithInfo SimpleUser -> C.ClientM AccessCodeDigits
confirmValidation :: String -> WithInfo AccessCodeDigits -> C.ClientM Value
validateUser :<|> confirmValidation :<|> getMovements = C.client (Proxy @AB)


-- * Types for marshalling

-- ** How to decode a movement table

data HTML
instance Accept HTML where
  contentType _ = "text/html"

instance MimeUnrender HTML [Movement] where
  mimeUnrender _ = maybe (Left "Couldn't parse movements table") Right . scrapeMT
    where
      stripUnpack = BSC.unpack . BSC.strip . BS.toStrict
      scrapeMT bs = scrapeStringLike bs $ do
        chroot ("div" @: ["id" @= "tableMovements"] // "table" // "tbody") $ do
          chroots "tr" $ do
            [_, dateStr]  <- texts ("td" @: [hasClass "date"])
            desc          <- text  ("td" @: [hasClass "desc"])
            [amount, bal] <- texts ("td" @: [hasClass "amount"])

            date <- parseTimeM False undefined "%d/%m/%0Y" (stripUnpack dateStr)

            pure $ Mov date (stripUnpack desc) (stripUnpack amount) (stripUnpack bal)

-- ** How to encode a user

data SimpleUser = SimpleUser String

instance ToJSON SimpleUser where
  toJSON (SimpleUser s) = object [ "user" .= s ]

-- ** How to receive and send access code digits

data AccessCodeDigits = AccessCodeDigits Int Int Int

instance FromJSON AccessCodeDigits where
  parseJSON = withObject "parsing access codes" $ \obj -> do
    d   <- obj .: "d"
    c6  <- parseJSON $ d V.! 2
    a:_ <- parseJSON $ c6 V.! 0
    b:_ <- parseJSON $ c6 V.! 1
    c:_ <- parseJSON $ c6 V.! 2
    pure $ AccessCodeDigits (digitToInt a) (digitToInt b) (digitToInt c)

instance ToJSON AccessCodeDigits where
  toJSON (AccessCodeDigits a b c) = object [ "code1" .= show a
                                           , "code2" .= show b
                                           , "code3" .= show c
                                           , "sms"   .= ("" :: String) ]

-- ** Wrapper to add required fingerprint and browserInfo information

-- | Copy fingerprint AND browserInfo from site
-- Haven't figured out a workaround for it yet
data WithInfo a = WithInfo
                    String -- ^ fingerprint
                    String -- ^ browserInfo
                    a

instance ToJSON a => ToJSON (WithInfo a) where
  toJSON (WithInfo f b a) = Object $ ("fingerprint" .= f) <> ("browserInfo" .= b) <> (case toJSON a of Object o -> o; _ -> error "expobj")

-- ** How to request for the movements table

data MovementsRequest
  = MovementsRequest
    { _movs_from :: Day
    , _movs_to   :: Day
    }

instance ToForm MovementsRequest where
  toForm (MovementsRequest f t) =
    [ ("Control"      , "AccountMovementControl")
    , ("AccountObject", "{}")
    , ("DateInit"     , toQueryParam (formatTime undefined "%d/%m/%0Y" f))
    , ("DateEnd"      , toQueryParam (formatTime undefined "%d/%m/%0Y" t))
    ]

