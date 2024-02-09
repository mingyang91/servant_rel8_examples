{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Lib
    ( someFunc
    , authorSchema
    , projectSchema
    , DBCtx(..)
    , HasDBPool(..)
    , DBM(..)
    , AuthorView(..)
    , ProjectView(..)
    , AuthorService(..)
    ) where


import           Prelude
import           Rel8
import           GHC.Generics
import           Data.Int(Int64)
import           Data.Text(Text)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, ask, asks, MonadIO (liftIO))
import Control.Monad.Trans.Either (EitherT, left, right)
import Control.Monad.Trans.Class (lift)
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import Control.Monad.Reader.Class (MonadReader (local))
import Hasql.Pool (Pool, use)
import Hasql.Connection (Connection)
import qualified Hasql.Pool as Pool
import qualified Rel8
import qualified Hasql.Statement
import Data.Aeson.Types (ToJSON, FromJSON)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Author f = Author
  { authorId   :: Column f AuthorId
  , authorName :: Column f Text
  , authorUrl  :: Column f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

newtype AuthorId = AuthorId { toInt64 :: Int64 }
  deriving newtype (DBEq, DBType, Eq, Show)

data Project f = Project
  { projectAuthorId :: Column f AuthorId
  , projectName     :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Author f)
deriving stock instance f ~ Result => Show (Project f)


authorSchema :: TableSchema (Author Name)
authorSchema = TableSchema
  { name = "author"
  , columns = Author
      { authorId = "author_id"
      , authorName = "name"
      , authorUrl = "url"
      }
  }

projectSchema :: TableSchema (Project Name)
projectSchema = TableSchema
  { name = "project"
  , columns = namesFromLabels @(Project Name)
  }

findAuthorById :: AuthorId -> Query (Author Expr)
findAuthorById aid = limit 1 $ do
  author <- each authorSchema
  where_ $ authorId author ==. lit aid
  pure author

data AuthorView = AuthorView
  { authorViewId   :: Int64
  , authorViewName :: Text
  , authorViewUrl  :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON AuthorView
instance FromJSON AuthorView

data DBCtx = DBCtx { _pool :: Pool }

newtype DBM ctx a = DBM {
  runDatabaseM :: ExceptT Pool.UsageError (ReaderT ctx IO) a
}

deriving instance MonadReader ctx (DBM ctx)
deriving instance Applicative (DBM ctx)
deriving instance Functor (DBM ctx)
deriving instance Monad (DBM ctx)
deriving instance MonadIO (DBM ctx)

class HasDBPool ctx where
  getPool :: ctx -> Pool

instance HasDBPool DBCtx where
  getPool = _pool

-- instance HasDBPool ctx => Has Pool ctx where 
--   getter = getPool


class Monad m => AuthorService m where
  getAuthorById :: Int64 -> m (Maybe AuthorView)
  listAuthors :: m [AuthorView]


projectsForAuthor a = each projectSchema >>= Rel8.filter \p ->
  projectAuthorId p ==. authorId a

authorsAndProjects = do
  author  <- each authorSchema
  project <- projectsForAuthor author
  return (author, project)

fetchAuthorById :: Pool -> AuthorId -> IO (Either Pool.UsageError (Maybe (Author Result)))
fetchAuthorById pool aid = Pool.use pool $ Session.statement () (Rel8.runMaybe (Rel8.select $ findAuthorById aid))

instance HasDBPool ctx => AuthorService (DBM ctx) where
  getAuthorById :: Int64 -> DBM ctx (Maybe AuthorView)
  getAuthorById aid = DBM $ do
    pool <- asks getPool
    result <- lift . liftIO $ fetchAuthorById pool $ AuthorId aid
    case result of
      Left err -> do
        lift . liftIO $ putStrLn $ "Error querying database: " ++ show err
        return Nothing -- Continue with Nothing on error
      Right maybeAuthor -> return $ fmap authorToAuthorView maybeAuthor
  
  listAuthors :: DBM ctx [AuthorView]
  listAuthors = DBM $ do
    pool <- asks getPool
    result <- lift . liftIO $ Pool.use pool $ Session.statement () (Rel8.run $ Rel8.select $ Rel8.each authorSchema)
    case result of
      Left err -> do
        lift . liftIO $ putStrLn $ "Error querying database: " ++ show err
        return [] -- Continue with empty list on error
      Right authors -> return $ fmap authorToAuthorView authors

authorToAuthorView :: (f ~ Result) => Author f -> AuthorView
authorToAuthorView author = AuthorView
  { authorViewId = toInt64 $ authorId author
  , authorViewName = authorName author
  , authorViewUrl = authorUrl author
  }

data ProjectView = ProjectView
  { projectViewAuthorId :: Int64
  , projectViewName     :: String
  }
  deriving stock (Generic)

instance ToJSON ProjectView
instance FromJSON ProjectView

class Monad m => ProjectService m where
  getProjectById :: AuthorId -> m (Maybe ProjectView)
  listProjects :: m [ProjectView]

instance ProjectService IO where
  getProjectById = undefined
  listProjects = undefined