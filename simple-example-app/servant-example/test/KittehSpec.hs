{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module KittehSpec (spec) where

import           App                  (app)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Network.Wai.Test (SResponse)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

-- | like post, but sets Content-Type to application/json
postJ :: ByteString -> BSL.ByteString -> WaiSession SResponse
postJ json = request "POST" json [("Content-Type", "application/json")]

spec :: Spec
spec = with app $ do
    describe "GET /kitteh" $ do
        it "responds with 200" $ do
            get "/kitteh" `shouldRespondWith` 200
        it "responds with [Kitteh]" $ do
            get "/kitteh" `shouldRespondWith` [json|[ {"kittehName":"Mittens","kittehColor":"Tabby"}
                                                    , {"kittehName":"Fluffy","kittehColor":"Orange"}
                                                    ] |]
    describe "POST /kitteh" $ do
      it "adds a new kitteh" $ do
        postJ "/kitteh" [json|{"kittehName":"Tigger","kittehColor":"Brown"}|]
          `shouldRespondWith` 200
        get "/kitteh" `shouldRespondWith`
            [json|[ {"kittehName":"Tigger","kittehColor":"Brown"}
                  , {"kittehName":"Mittens","kittehColor":"Tabby"}
                  , {"kittehName":"Fluffy","kittehColor":"Orange"}
                  ] |]
      it "replaces an existing kitteh" $ do
        postJ "/kitteh" [json|{"kittehName":"Fluffy","kittehColor":"Black"}|]
          `shouldRespondWith` 200
        get "/kitteh" `shouldRespondWith`
            [json|[ {"kittehName":"Fluffy","kittehColor":"Black"}
                  , {"kittehName":"Mittens","kittehColor":"Tabby"}
                  ] |]
