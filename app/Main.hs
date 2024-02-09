{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where


import           Data.Aeson
import           Data.ByteString.Char8 (pack, ByteString)
import           Data.Has
import           Data.String.Conversions (cs)
import           Data.Time.Clock (DiffTime)
import           Control.Exception ( bracket )
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import qualified Control.Monad.IO.Class
import           Control.Monad (void, (<=<))

import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified Servant
import           System.IO

import           Lib ( someFunc, authorSchema, DBM, AuthorService(getAuthorById, listAuthors), AuthorView(AuthorView), ProjectView(ProjectView) )
import           Hasql.Pool ( Pool, acquire, release )
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Except (ExceptT, MonadError)
import           Data.Int (Int64)


-- * api

type ItemApi =
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item :<|>
  "author" :> Get '[JSON] [AuthorView] :<|>
  "author" :> Capture "authorId" Integer :> Get '[JSON] AuthorView :<|>
  "project" :> Get '[JSON] [ProjectView] :<|>
  "project" :> Capture "projectId" Integer :> Get '[JSON] ProjectView

type ItemApi2 =
  "author" :> Capture "authorId" Int64 :> Get '[JSON] AuthorView

itemApi :: Proxy ItemApi
itemApi = Proxy

main :: IO ()
main = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  withPool (runSettings settings <=< mkApp)

-- connectToDb = do

--   _ <- bracket (either (error . show) return =<< acquire (pack "postgresql://postgres@localhost/postgres")) release \conn -> void do
--     flip run conn $ do
--       sql "CREATE EXTENSION citext"
--       sql "CREATE TABLE test_table ( column1 text not null, column2 bool not null )"
--       sql "CREATE TABLE unique_table ( \"key\" text not null unique, \"value\" text not null )"
--       sql "CREATE SEQUENCE test_seq"
--       sql "CREATE TYPE composite AS (\"bool\" bool, \"char\" char, \"array\" int4[])"

--   return ""


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


getAuthorByIdFromDB :: AuthorService m => Int64 -> m AuthorView
getAuthorByIdFromDB aid = undefined
-- do
--   maybeAuthorView <- getAuthorById aid
--   return $ case maybeAuthorView of
--     Just authorView -> authorView
--     Nothing -> throwError err404 { errBody = cs "Author not found" }


server2 :: AuthorService m => ServerT ItemApi2 m
server2 = getAuthorByIdFromDB


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
