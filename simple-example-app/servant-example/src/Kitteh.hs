{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Kitteh where

import           Control.Monad.Trans
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Function
import           Data.IORef
import qualified Data.List                as List
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Servant

data Kitteh = Kitteh
  { kittehName  :: Text
  , kittehColor :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Kitteh)

type KittehAPI = "kitteh" :> (Get '[JSON] [Kitteh]
                              :<|> ReqBody '[JSON] Kitteh :> Post '[JSON] NoContent
                             )

-- | List of cats when we start the application
defaultKittehs :: [Kitteh]
defaultKittehs = [ Kitteh "Mittens" "Tabby"
                 , Kitteh "Fluffy" "Orange"
                 ]

serveKittehAPI :: IORef [Kitteh] -> Server KittehAPI
serveKittehAPI kittehs =
  liftIO (readIORef kittehs)
  -- When we receive a new cat, we store it at the front of the list and remove
  -- duplicates (by name)
  :<|> (\kitteh -> liftIO $ atomicModifyIORef kittehs
                   (\ks -> (List.nubBy ((==) `on` kittehName) ( kitteh : ks), NoContent)))
