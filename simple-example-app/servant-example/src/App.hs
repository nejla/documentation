{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module App
    ( startApp
    , app
    ) where

import Data.IORef
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Kitteh

type API = KittehAPI

startApp :: IO ()
startApp = run 8080 =<< app

app = do
  kittehs <- newIORef defaultKittehs
  return $ serve api (server kittehs)

api :: Proxy API
api = Proxy

server :: IORef [Kitteh] -> Server API
server = serveKittehAPI
