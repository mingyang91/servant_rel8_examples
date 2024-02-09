{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where


import           Data.Has
import           Data.ByteString.Char8 (pack, ByteString)
import           Control.Exception ( bracket )
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader

import           Data.Aeson
import           Data.Time.Clock (DiffTime)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified Servant
import           System.IO

import           Lib ( someFunc, authorSchema, getAuthorById )
import qualified Hasql.Connection
import           Hasql.Pool ( Pool, use, acquire, release )
import           Hasql.Session ( Session, sql )
import qualified Rel8
import           Control.Monad (void, (<=<))
import           Distribution.Compat.Binary (Binary(put))
import qualified Control.Monad.IO.Class
import qualified Rel8


-- * api

type ItemApi =
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item :<|>
  "author" :> Get '[JSON] [Author] :<|>
  "author" :> Capture "authorId" Integer :> Get '[JSON] Author :<|>
  "project" :> Get '[JSON] [Project] :<|>
  "project" :> Capture "projectId" Integer :> Get '[JSON] Project

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
  getAuthorByIdFromDB :<|>
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

getAuthors :: Handler [Author]
getAuthors = return [exampleAuthor]

-- Handler but tagless final style, connection
-- getAuthorsFromDb :: (MonadReader m r, Has Session r, MonadIO m) => m [Author]
-- getAuthorsFromDb = do
--   pool <- ask
--   return [exampleAuthor]

getAuthorByIdFromDB :: Integer -> Handler Author
getAuthorByIdFromDB aid = return exampleAuthor


exampleAuthor :: Author
exampleAuthor = Author 0 "example author"

getProjects :: Handler [Project]
getProjects = return [exampleProject]

getProjectById :: Integer -> Handler Project
getProjectById = \ case
  0 -> return exampleProject
  _ -> throwError err404

exampleProject :: Project
exampleProject = Project 0 "example project"

-- * item

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item

data Author
  = Author {
    authorId :: Integer,
    authorName :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Author
instance FromJSON Author

data Project
  = Project {
    projectId :: Integer,
    projectName :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Project
instance FromJSON Project

