{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}

import Options.Applicative
import Debug.Trace (trace, traceShow)
import Data.Either
import Data.Decimal
import Data.Time.Calendar
import Data.Time.Format
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Char
import Data.Proxy
import Data.String
import Control.Monad.IO.Class
import System.Directory
import System.FilePath
import Servant.Client.Core (ClientError(..), ResponseF(..))
import Control.Exception

import Control.Events hiding (Rules(..))
import Hledger
import Hledger.Read.RulesReader (readRules, readJournalFromCsv)

import ActivoBank

--------------------------------------------

-- | Runs daily, so the next run is expected within 25h.
scrapeActivoBank :: Integer {-^ Fetch movements from X days back to now-} -> [Int] -> String -> String -> String -> IO ()
scrapeActivoBank daysBack codes user fingerprint browserI =
  withConn (script <> "finances") $ \c ->
  event c "hledger-activobank" (simple ("hledger-activobank -d " ++ show daysBack) & evtExpected ?~ 25 * 3600) $ \ev -> do

    journal <- defaultJournal
    rules <- either fail pure =<< runExceptT (readRules (takeDirectory (journalFilePath journal) </> "activobank.rules"))

    mvs <- event c "fetch-movements" (simple "Fetch movements table" & scoped ?~ ev) $ \_ -> do

      -- Get movements from activo bank
      mvs <- try (withSession codes user fingerprint browserI (fetchMovementsTable daysBack))
      case mvs of
        Left (FailureResponse _rq rsp)
          | let bodyT = T.decodeUtf8 $ BS.toStrict $ responseBody rsp
                 -> do
                    T.putStr bodyT
                    pure $ failed (T.unpack bodyT) mvs
        Left e   -> pure $ failed (displayException e) mvs
        Right ms -> pure $ done ("Fetched " ++ show (length ms) ++ " movements") mvs

    case mvs of
      Left e -> throwIO e
      Right movements -> do

        -- Add movements to HLedger if they are new
        newTransactions <- either fail (pure . map (styleAmounts (journalCommodityStyles journal)) . jtxns)
          =<< runExceptT (readJournalFromCsv rules "(activobank-hs-scraper)" (movementsCsv (reverse (filter (isNew journal) movements))) Nothing)

        mapM_ (T.putStr . showTransaction) newTransactions
        mapM_ (T.appendFile (journalFilePath journal) . showTransaction) newTransactions

        pure $ done "Updated ledger successfully" ()

--------------------------------------------

isNew :: Journal -> Movement -> Bool
isNew journal (Mov day (fromString -> dd) amt _bal) =
  case journalLastDay False{-primary date-} journal of
    -- Only add transactions from last day in the journal
    -- and only if there does not exist a similar transaction with the same amount
    Just lastDay -> lastDay <= day && null
      (journalTransactionsSimilarTo journal dd
        (And [ Date $ DateSpan (Just (Exact day)) (Just (Exact (addDays 1 day)))
             , Amt Eq (realFracToDecimal 2 amt)
             ]) 0.8 1)
    Nothing -> False

-- | Movements in the CSV format expected by activobank.rules
movementsCsv :: [Movement] -> T.Text
movementsCsv ms = T.unlines $ "date;value_date;description;amount;balance" : map row ms
  where
    row (Mov day desc amt bal) = T.intercalate ";" [d, d, quote desc, money amt, money bal]
      where d = T.pack (formatTime defaultTimeLocale "%d/%m/%Y" day)
    quote s = "\"" <> T.replace "\"" "\"\"" (T.pack s) <> "\""
    money = T.replace "." "," . T.pack . show . realFracToDecimal 2

--------------------------------------------

daysBackOpt :: Parser Integer
daysBackOpt =
  option auto
   $ long "days-back"
  <> short 'd'
  <> metavar "INT"
  <> help "The number of days back to fetch movements from."
  <> Options.Applicative.value 7
  <> showDefault

main :: IO ()
main = do

  daysBack <- execParser $ info (daysBackOpt <**> helper <**> simpleVersioner "v0.3") mempty

  xdgConfigDir <- getXdgDirectory XdgConfig "activobank"

  -- Sessions
  -- browserInfo and fingerprint are part of the basic request body to login
  -- REMEMBER TO check for browserInfo and fingerprint in the request body of
  -- the login requests, but DO IT IN A PRIVATE SESSION, TO MAKE SURE
  -- CACHE/COOKIES ARE CLEAN, or otherwise fingerprint shows up empty
  [map digitToInt -> codes, user, fingerprint, browserI] <- lines <$> readFile (xdgConfigDir </> ".activobank.secret")

  scrapeActivoBank daysBack codes user fingerprint browserI

