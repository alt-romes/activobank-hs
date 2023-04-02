{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

import Debug.Trace (traceShow)
import Data.Either
import Data.Time.Calendar
import qualified Data.Text.IO as T
import Data.Char
import Data.Proxy
import Data.String
import Control.Monad.IO.Class

import Hledger

import ActivoBank

--------------------------------------------

scrapeActivoBank :: Journal -> [Int] -> String -> String -> String -> IO ()
scrapeActivoBank journal codes user fingerprint browserI = do

  -- Get movements from activo bank
  movements <- withSession codes user fingerprint browserI (fetchMovementsTable 50)

  -- Add movements to HLedger if they are new
  let newTransactions = foldl (insertIfNew journal) [] movements

  mapM_ (T.putStr . showTransaction) newTransactions
  mapM_ (T.appendFile (journalFilePath journal) . showTransaction) newTransactions

--------------------------------------------

insertIfNew :: Journal -> [Transaction] -> Movement -> [Transaction]
insertIfNew journal trs mov@(Mov day (fromString -> dd) amt bal) =
    -- FIXME: If there are two transactions with the same description on the
    -- same day, we will ignore them, but only the second time this is run the
    -- same day. Does it even matter? Only if we delete an entry we previously logged?
  if null $ filter ((> 0.5) . fst) $
            journalTransactionsSimilarTo journal
              (And [ Desc   (either (\e -> traceShow e (toRegex' ".*")) id $ toRegex dd)
                   , Date $ DateSpan (Just (Exact day)) (Just (Exact (addDays 1 day)))
                   ]) dd 1
    then (toTransaction mov):trs
    else trs

toTransaction :: Movement -> Transaction
toTransaction (Mov day dd (toRational -> amt) bal) =
  (transaction day
  [ post "Assets:Checking:ActivoBank" $ Amount "€" (fromRational amt) amountstyle{ascommodityside=R} Nothing
  , post (if amt > 0 then "Income" else "Expenses") $ Amount "€" (- (fromRational amt)) amountstyle{ascommodityside=R} Nothing
  ]) { tdescription = fromString dd }

--------------------------------------------

main :: IO ()
main = do
  -- Sessions
  [map digitToInt -> codes, user, fingerprint, browserI] <- lines <$> readFile "activobank.secret"

  journal <- defaultJournal

  scrapeActivoBank journal codes user fingerprint browserI
