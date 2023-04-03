{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}

import qualified Data.List.NonEmpty as NE
import Debug.Trace (trace, traceShow)
import Data.Either
import Data.Decimal
import Data.Time.Calendar
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Char
import Data.Proxy
import Data.String
import Control.Monad.IO.Class
import System.Directory
import System.FilePath

import Hledger

import ActivoBank

-- | A mapping from a query on transaction to the account associated with the
-- transaction (on the other end)
type Rules = [(Query,AccountName)]

--------------------------------------------

scrapeActivoBank :: Journal -> Rules -> [Int] -> String -> String -> String -> IO ()
scrapeActivoBank journal rules codes user fingerprint browserI = do

  -- Get movements from activo bank
  movements <- withSession codes user fingerprint browserI (fetchMovementsTable 7)

  -- Add movements to HLedger if they are new
  let newTransactions = foldl (insertIfNew journal rules) [] movements

  mapM_ (T.putStr . showTransaction) newTransactions
  mapM_ (T.appendFile (journalFilePath journal) . showTransaction) newTransactions

--------------------------------------------

insertIfNew :: Journal -> Rules -> [Transaction] -> Movement -> [Transaction]
insertIfNew journal rules trs mov@(Mov day (fromString -> dd) amt bal) =
    -- FIXME: If there are two transactions with the same description on the
    -- same day, we will ignore them, but only the second time this is run the
    -- same day. Does it even matter? Only if we delete an entry we previously logged?
  if not $ any ((> 0.75) . fst) $
            journalTransactionsSimilarTo journal
              (And [ Desc   (either (\e -> traceShow e (toRegex' ".*")) id $ toRegex dd)
                   , Date $ DateSpan (Just (Exact day)) (Just (Exact (addDays 1 day)))
                   ]) dd 1
    then toTransaction rules mov:trs
    else trs

toTransaction :: Rules -> Movement -> Transaction
toTransaction rules (Mov day dd (realFracToDecimal 2 -> amt) bal) =
  (transaction day [abPost, otherPost]) { tdescription = fromString ("* " <> dd) }
    where
      abPost = post "Assets:Checking:ActivoBank" $ Amount "€" amt amtstyle Nothing
      tmpTransaction = (transaction day [abPost]) { tdescription = fromString dd }
      otherPost = post (case NE.nonEmpty (filter ((`matchesTransaction` tmpTransaction) . fst) rules) of
                          Just ((_,acc) NE.:| _) -> acc
                          Nothing -> if amt > 0 then "Revenue:TODO" else "Expenses:TODO")
                    $ Amount "€" (-amt) amtstyle Nothing
      amtstyle = amountstyle{ascommodityside=R,asprecision=NaturalPrecision}

--------------------------------------------

main :: IO ()
main = do
  xdgConfigDir <- getXdgDirectory XdgConfig "activobank"

  -- Sessions
  [map digitToInt -> codes, user, fingerprint, browserI] <- lines <$> readFile (xdgConfigDir </> ".activobank.secret")
  rules <- map ((\(qstr, acc) -> (either error fst (parseQuery nulldate qstr), T.drop 4 acc)) . T.breakOn "==> ")
              . T.lines <$> T.readFile (xdgConfigDir </> "rules.txt")            -- Match ^ characters, ==> and the space afterwards

  journal <- defaultJournal

  scrapeActivoBank journal rules codes user fingerprint browserI

