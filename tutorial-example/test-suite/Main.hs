{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Data           (Proxy(..))
import Test.Hspec
import Test.Hspec.Wai      as Wai
import Test.Hspec.Wai.JSON (json)

import NejlaCommon.Test    as NC

import RunMain             ( AppState(..) , AppConfig(..)
                           , migrate, serveApp
                           )

testAppState :: AppState
testAppState =
  AppState
  { appStateConfig =
      AppConfig
      { appConfigEmail = "test@example.com"
      }
  }

specs :: DBApiSpec ()
specs = do
  describe "/users" $ do
    it "accepts a POST request" $ do
      postJ "/users"
        [json|{ "name": "Robert"
              , "points": 33
              , "group": "development"
              }
             |] `shouldRespondWith` 201
    it "GETs the user" $ do
      postJ "/users"
        [json|{ "name": "Robert"
              , "points": 33
              , "group": "development"
              }
             |] `shouldRespondWith` 201

      users <- get "/users" `shouldReturnA` (Proxy @NC.JSON)
      users `NC.shouldBe` [json|[{ "name": "Robert"
                                 , "points": 33
                                 , "group": "development"
                                 }]
                               |]

main :: IO ()
main = do
  connectInfo <- dbTestConnectInfo
  specApi connectInfo migrate mkApi specs
  where
    mkApi pool run =
      liftIO $ run () (serveApp pool testAppState)
