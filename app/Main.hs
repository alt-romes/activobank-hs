{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Char
import Data.Proxy
import Control.Monad.IO.Class

import System.Cron

import Cob.RecordM.TH
import Cob.RecordM.Query
import Cob.RecordM.DateTime
import Cob.UserM
import Cob.Session
import Cob

import Servant.API
import Servant.Server
import qualified Network.Wai.Handler.Warp as Warp

import ActivoBank

--------------------------------------------

data MovementRM = MovementRM
                    { dataMov :: DateTime
                    , desc    :: String
                    , movm    :: Double
                    , saldo   :: Double
                    } deriving Show
mkRecord ''MovementRM "CASA Finanças Movimentos" ["Data mov", "Descritivo", "Movimento", "Saldo"]

--------------------------------------------

scrapeActivoBank :: CobSession -> [Int] -> String -> IO ()
scrapeActivoBank cobsession codes user = do

  -- Get movements from activo bank
  movements <- withSession codes user "" "" (fetchMovementsTable 5)

  -- Add movements to RecordM
  mockCob cobsession $ do

    mapM_ (insertIfNew . toCobMov) (take 1 movements)

--------------------------------------------

insertIfNew :: MovementRM -> Cob ()
insertIfNew mov@(MovementRM dt dd amt bal) =

  let
      q =  "data_mov"   =: dt
        <> "descritivo" =: dd
        <> "movimento"  =: amt
        <> "saldo"      =: bal

   in do
     
      n <- count @MovementRM q

      if n == 0

        then do

          add mov
          return ()

        else return ()

toCobMov :: Movement -> MovementRM
toCobMov (Mov day dd amt bal) = MovementRM (dateTimeFromDay day) dd amt bal

--------------------------------------------

type PushNoti = "scrape" :> "activobank" :> Get '[PlainText] NoContent

main :: IO ()
main = do
  -- Sessions
  [map digitToInt -> codes, user] <- lines <$> readFile "activobank.secret"
  [host, cobuser, pass] <- lines <$> readFile "cob.secret"
  cs                    <- umSession host cobuser pass -- todo: a sessão expira eventualmente?

  -- Schedule cron job
  execSchedule $ do
    addJob (scrapeActivoBank cs codes user) "0 8,12,16,20 * * *"
  
  -- Run HTTP server
  Warp.run 40013 $ serve (Proxy @PushNoti) (NoContent <$ liftIO (scrapeActivoBank cs codes user))

