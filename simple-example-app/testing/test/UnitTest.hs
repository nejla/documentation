{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module UnitTest where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans
import Database.Persist.Sqlite

import Test.Hspec

import Persistent

withTestDB f = runNoLoggingT . withSqliteConn ":memory:" $ \backend -> lift $
  runReaderT ( do
      runMigrationSilent migrateAll    -- Don't want chatter between tests
      f
             ) backend


spec = do
  describe "transactionTotal" $ do
    it "returns the sum of transactions" . withTestDB $ do
      insertTransaction "a1" 200
      insertTransaction "a1" 300

      total <- transactionTotal "a1"

      liftIO $ total `shouldBe` 500
    it "is unaffected by transactions in other accounts" . withTestDB $ do
      insertTransaction "a1" 200
      insertTransaction "a1" 300

      insertTransaction "a2" 300

      total <- transactionTotal "a1"
      liftIO $ total `shouldBe` 500

spec_fixed = do
  describe "transactionTotal" $ do
    it "returns the sum of transactions" . withTestDB $ do
      insertTransaction "a1" 200
      insertTransaction "a1" 300

      total <- transactionTotal_fixed "a1"

      liftIO $ total `shouldBe` 500
    it "is unaffected by transactions in other accounts" . withTestDB $ do
      insertTransaction "a1" 200
      insertTransaction "a1" 300

      insertTransaction "a2" 300

      total <- transactionTotal "a1"
      liftIO $ total `shouldBe` 500


main = hspec spec
