{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Persistent where

import           Control.Monad.Reader
import           Control.Monad.Trans
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Transaction
    account Text
    amount Int
    deriving Show
|]


type App a = ReaderT SqlBackend IO a

-- Insert a transaction
insertTransaction :: Text -- ^ Name of the account the transaction belongs to
                  -> Int -- ^ Amount Changed
                  -> App ()
insertTransaction name amount = do
  _ <- insert $ Transaction name  amount
  return ()

-- | Calculate the total transactions
transactionTotal :: Text -- ^ Name of the account
                 -> App Int
transactionTotal name = do
  ts <- selectList [TransactionAccount !=. name] []
  return . sum $ for ts $ \(Entity _ (Transaction _name amnt)) -> amnt
  where
    for = flip map

-- | Calculate the total transactions
transactionTotal_fixed :: Text -- ^ Name of the account
                 -> App Int
transactionTotal_fixed name = do
  ts <- selectList [TransactionAccount ==. name] []
  return . sum $ for ts $ \(Entity _ (Transaction _name amnt)) -> amnt
  where
    for = flip map
