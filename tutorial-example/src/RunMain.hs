{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE StrictData #-}

module RunMain
  ( module RunMain
  , ConnectionPool
  )

where

import           Control.Monad.Reader
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           NejlaCommon                 (db')
import qualified NejlaCommon                 as NC
import           NejlaCommon.Helpers
import qualified Network.Wai.Handler.Warp    as Warp
import           Servant
import           Servant.Server

import           Control.Monad.Logger        (runStderrLoggingT, MonadLogger)
import qualified Data.Aeson.TH               as Aeson
import           Data.Default                (Default(def))
import           Database.Persist            as P
import           Database.Persist.Postgresql
                 -- Use ConnectInfo from postgresql-simple for more stuctured generation of connection string
import           Database.Persist.TH
import           Database.PostgreSQL.Simple  ( ConnectInfo(..)
                                            , postgreSQLConnectionString)
import           NejlaCommon.Config

share [ mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  User
    name Text
    points Int
    group Text
   |]

migrate :: MonadIO m => ReaderT SqlBackend m ()
migrate = runMigration migrateAll

Aeson.deriveJSON (aesonTHOptions "user") ''User

data AppConfig = AppConfig
  { appConfigEmail :: Text -- just an example
  }

data AppState = AppState
  { appStateConfig :: AppConfig
  }

type AddUserApi = "users"
                :> ReqBody '[ JSON ] User
                :> PostCreated '[ JSON ] NoContent

addUserHandler :: ConnectionPool -> AppState -> Server AddUserApi
addUserHandler pool conf user = do
  run conf pool $ do
    _ <- db' $ P.insert user
    return ()
  return NoContent

type GetUsersApi = "users" :> Get '[ JSON ] [User]

getUsersHandler :: ConnectionPool -> AppState -> Server GetUsersApi
getUsersHandler pool conf = do
  usrs <- run conf pool $ db' (P.selectList [] [])
  return $ P.entityVal <$> usrs

type Api = AddUserApi
         :<|> GetUsersApi

api :: Proxy Api
api = Proxy

handler :: ConnectionPool -> AppState -> Server Api
handler pool conf = addUserHandler pool conf
                  :<|> getUsersHandler pool conf

type App = NC.App AppState 'NC.Privileged 'NC.ReadCommitted

run :: MonadIO m => AppState -> ConnectionPool -> App a -> m a
run conf pool f = liftIO $ NC.runApp' def pool conf f

parseDatabaseConf :: (MonadLogger m, MonadIO m) => Config -> m ConnectionString
parseDatabaseConf conf = do
   dbHost <- getConf "DB_HOST" "db.host" (Right "localhost") conf
   dbPort <- getConf' "DB_PORT" "db.port" (Right 5432) conf -- uses read instance
   dbUser <- getConf "DB_USER" "db.user" (Left "database user name") conf -- will abort Program when value is unset
   dbPassword <- getConf "DB_PASSWORD" "db.password" (Right "") conf
   dbDatabase <- getConf "DB_DATABASE" "db.database" (Right dbUser) conf
   return . postgreSQLConnectionString $
     ConnectInfo { connectHost = Text.unpack dbHost
                 , connectPort = dbPort
                 , connectUser = Text.unpack dbUser
                 , connectPassword = Text.unpack dbPassword
                 , connectDatabase = Text.unpack dbDatabase
                 }

parseAppConfig :: (MonadLogger m, MonadIO m) => Config -> m AppConfig
parseAppConfig conf = do
  email <- getConf "APP_EMAIL" "app.email" (Left "Email Address") conf
  return AppConfig { appConfigEmail = email
                   }

warpSettings :: Int -> Warp.Settings
warpSettings port =
    -- Give open connections 1 second to finish running.
    Warp.setGracefulShutdownTimeout (Just 1)
  $ Warp.setPort port Warp.defaultSettings

serveApp :: ConnectionPool -> AppState -> Application
serveApp pool conf = serve api $ handler pool conf

runMain :: IO ()
runMain = runStderrLoggingT $ do
  conf <- loadConf "my-app"
  dbConString <- parseDatabaseConf conf
  appConf <- parseAppConfig conf
  port <- getConf' "PORT" "port" (Right 80) conf
  withPostgresqlPool dbConString 5 $ \pool -> do
     _ <- NC.runPoolRetry pool $ runMigration migrateAll
     let appState = AppState { appStateConfig = appConf }
     liftIO $ Warp.runSettings
                (warpSettings port)
                (serveApp pool appState)
