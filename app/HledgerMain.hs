{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

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
  movements <- withSession codes user fingerprint browserI (fetchMovementsTable 7)

  -- Add movements to HLedger if they are new
  let newTransactions = foldl (insertIfNew journal) [] movements

  mapM_ (T.putStr . showTransaction) newTransactions
  mapM_ (T.appendFile (journalFilePath journal) . showTransaction) newTransactions

--------------------------------------------

insertIfNew :: Journal -> [Transaction] -> Movement -> [Transaction]
insertIfNew journal trs mov@(Mov day (fromString -> dd) amt bal) =
  if null $ filter ((> 0.95) . fst) (journalTransactionsSimilarTo journal Any dd 1)
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
