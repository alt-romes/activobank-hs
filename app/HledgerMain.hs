{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}

import Options.Applicative
import qualified Data.List.NonEmpty as NE
import Debug.Trace (trace, traceShow)
import Data.Either
import Data.Decimal
import Data.Time.Calendar
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Char
import Data.Proxy
import Data.String
import Control.Monad.IO.Class
import System.Directory
import System.FilePath
import Servant.Client.Core (ClientError(..), ResponseF(..))
import Control.Exception

import Hledger

import ActivoBank

-- | A mapping from a query on transaction to the account
-- associated with the transaction (on the other end)
type Rules = [(Query,AccountName)]

--------------------------------------------

scrapeActivoBank :: Integer {-^ Fetch movements from X days back to now-} -> Journal -> Rules -> [Int] -> String -> String -> String -> IO ()
scrapeActivoBank daysBack journal rules codes user fingerprint browserI = do

  -- Get movements from activo bank
  movements <- withSession codes user fingerprint browserI (fetchMovementsTable daysBack) `catch` \case e@(FailureResponse _rq rsp) -> BS.putStr (responseBody rsp) >> throwIO e; e -> throwIO e

  -- Add movements to HLedger if they are new
  let newTransactions = foldl (insertIfNew journal rules) [] movements

  mapM_ (T.putStr . showTransaction) newTransactions
  mapM_ (T.appendFile (journalFilePath journal) . showTransaction) newTransactions

--------------------------------------------

insertIfNew :: Journal -> Rules -> [Transaction] -> Movement -> [Transaction]
insertIfNew journal rules acc mov@(Mov day (fromString -> dd) amt _bal) =
  case journalLastDay False{-primary date-} journal of
    Just lastDay
      -- Only add transactions from last day in the journal
      | lastDay <= day
      -- and only if there does not exist a similar transaction with the same amount
      , null $
          journalTransactionsSimilarTo journal dd
            (And [ Date $ DateSpan (Just (Exact day)) (Just (Exact (addDays 1 day)))
                 , Amt Eq (realFracToDecimal 2 amt)
                 ]) 0.8 1
      -> toTransaction rules mov:acc
    _ -> acc

toTransaction :: Rules -> Movement -> Transaction
toTransaction rules (Mov day dd (realFracToDecimal 2 -> amt) (realFracToDecimal 2 -> bal)) =
  (transaction day [abPost, otherPost]) { tdescription = fromString ("* " <> dd) }
    where
      abPost = post' "Assets:Checking:ActivoBank" (mkEurAmt amt) (balassert (mkEurAmt bal))
      tmpTransaction = (transaction day [abPost]) { tdescription = fromString dd }
      otherPost = post (case NE.nonEmpty (filter ((`matchesTransaction` tmpTransaction) . fst) rules) of
                          Just ((_,acc) NE.:| _) -> acc
                          Nothing -> if amt > 0 then "Income:TODO" else "Expenses:TODO")
                    $ mkEurAmt (-amt)
      mkEurAmt x = Amount "€" x eurstyle Nothing
      eurstyle = amountstyle{ascommodityside=R,asprecision=NaturalPrecision}

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
  rules <- map ((\(qstr, acc) -> (either error fst (parseQuery nulldate qstr), T.drop 4 acc)) . T.breakOn "==> ")
              . T.lines <$> T.readFile (xdgConfigDir </> "rules.txt")            -- Match ^ characters, ==> and the space afterwards

  journal <- defaultJournal

  scrapeActivoBank daysBack journal rules codes user fingerprint browserI

