{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where


import           Data.Aeson
import           Data.ByteString.Char8 (pack, ByteString)
import           Data.String.Conversions (cs)
import           Data.Time.Clock (DiffTime)
import           Control.Exception ( bracket )
import           Control.Monad.Reader
import           Control.Monad ((<=<))
import           Control.Monad.Trans.Except (runExceptT, ExceptT (ExceptT), mapExcept, withExceptT, mapExceptT)
import           Control.Monad.Except (ExceptT, MonadError)
import           Control.Monad.Trans.Reader as TReader

import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import           Hasql.Pool ( Pool, acquire, release )
import           Data.Int (Int64)
import qualified Hasql.Pool as Pool
import           Lib ( someFunc
                     , authorSchema
                     , DBM (runDatabaseM)
                     , AuthorService( getAuthorById
                                    , listAuthors
                                    )
                     , AuthorView(AuthorView)
                     , ProjectView(ProjectView)
                     , runDatabaseM, HasDBPool (..)
                     )
import GHC.Base (liftM)
import Data.Has


-- * api

type ItemApi =
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item :<|>
  "author" :> Get '[JSON] [AuthorView] :<|>
  "author" :> Capture "authorId" Integer :> Get '[JSON] AuthorView :<|>
  "project" :> Get '[JSON] [ProjectView] :<|>
  "project" :> Capture "projectId" Integer :> Get '[JSON] ProjectView

itemApi :: Proxy ItemApi
itemApi = Proxy

type ItemApi2 =
  "author" :> Capture "authorId" Int64 :> Get '[JSON] AuthorView

itemApi2 :: Proxy ItemApi2
itemApi2 = Proxy

main :: IO ()
main = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  withPool (runSettings settings <=< mkApp)


data AppCtx = AppCtx {
  _pool :: Pool
}

instance HasDBPool AppCtx where
  getPool = _pool

mkApp :: Pool -> IO Application
mkApp pool = return $ serve itemApi $ server pool

acquireTimeout :: DiffTime
acquireTimeout = 10
lifetime :: DiffTime
lifetime = 60
idletime :: DiffTime
idletime = 10

connectionSettings :: ByteString
connectionSettings = pack "postgresql://postgres@localhost:5432/postgres"

withPool :: (Pool -> IO a) -> IO a
withPool = bracket (acquire 10 acquireTimeout lifetime idletime connectionSettings) release


server :: Pool -> Server ItemApi
server pool =
  getItems :<|>
  getItemById :<|>
  getAuthors :<|>
  mockGetAuthorById :<|>
  getProjects :<|>
  getProjectById

getItems :: Handler [Item]
getItems = return [exampleItem]

getItemById :: Integer -> Handler Item
getItemById = \ case
  0 -> return exampleItem
  _ -> throwError err404

exampleItem :: Item
exampleItem = Item 0 "example item"

getAuthors :: Handler [AuthorView]
getAuthors = return [exampleAuthor]

mockGetAuthorById :: Integer -> Handler AuthorView
mockGetAuthorById = \ case
  0 -> return exampleAuthor
  _ -> throwError err404

-- Handler but tagless final style, connection
-- getAuthorsFromDb :: (MonadReader m r, Has Session r, MonadIO m) => m [Author]
-- getAuthorsFromDb = do
--   pool <- ask
--   return [exampleAuthor]

  -- maybeAuthor <- test2 aid
  -- case maybeAuthor of
  --   Just author -> return author
  --   Nothing -> throwError err404

newtype AppM a = AppM {
  runAppM :: ExceptT Servant.ServerError (ReaderT AppCtx IO) a
} deriving newtype (Monad, Applicative, Functor, MonadIO, MonadReader AppCtx)

class Monad m => AppService m where
  getAuthorByIdHandler :: Int64 -> m AuthorView

instance AppService AppM where
  getAuthorByIdHandler :: Int64 -> AppM AuthorView
  getAuthorByIdHandler aid = AppM $ mapErrorToServerError $ runDatabaseM $ getAuthorById aid

mapErrorToServerError :: ExceptT Pool.UsageError (ReaderT AppCtx IO) (Maybe b) -> ExceptT ServerError (ReaderT AppCtx IO) b
mapErrorToServerError = mapExceptT $ fmap $ either
  (\err -> Left err500 { errBody = cs ("Internal Server Error" ++ show err) }) $
  maybe (Left err404 { errBody = cs "Author not found" }) Right

server2 :: AppService m => ServerT ItemApi2 m
server2 = getAuthorByIdHandler

mkApp2 ctx = serveWithContext itemApi2 ctx $
  hoistServerWithContext itemApi2 (Proxy :: Proxy '[AppCtx])
  (`runReaderT` ctx) server2

exampleAuthor :: AuthorView
exampleAuthor = AuthorView 0 (cs "example author") (Just $ cs "example url")

getProjects :: Handler [ProjectView]
getProjects = return [exampleProject]

getProjectById :: Integer -> Handler ProjectView
getProjectById = \ case
  0 -> return exampleProject
  _ -> throwError err404

exampleProject :: ProjectView
exampleProject = ProjectView 0 "example project"

-- * item

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item
