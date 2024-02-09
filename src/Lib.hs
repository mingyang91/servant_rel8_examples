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


module Lib
    ( someFunc
    , authorSchema
    , projectSchema
    , getAuthorById
    ) where


import           Prelude
import           Rel8
import           GHC.Generics
import           Data.Int(Int64)
import           Data.Text(Text)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, ask, MonadIO (liftIO))
import Control.Monad.Trans.Either (EitherT, left, right)
import Control.Monad.Trans.Class (lift)
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import Control.Monad.Reader.Class (MonadReader)
import Hasql.Pool (Pool, use)
import Hasql.Connection (Connection)
import qualified Hasql.Pool as Pool
import qualified Rel8
import qualified Hasql.Statement

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
  { authorViewId   :: AuthorId
  , authorViewName :: Text
  , authorViewUrl  :: Maybe Text
  }
  deriving stock (Generic)

newtype DBM a = DBM {
  runSessionM :: EitherT Pool.UsageError (ReaderT Pool IO) a
} deriving newtype (Monad, Applicative, Functor, MonadIO, MonadReader Pool)

class Monad m => AuthorService m where
  getAuthorById :: AuthorId -> m (Maybe AuthorView)
  listAuthors :: m [AuthorView]

projectsForAuthor a = each projectSchema >>= Rel8.filter \p ->
  projectAuthorId p ==. authorId a

authorsAndProjects = do
  author  <- each authorSchema
  project <- projectsForAuthor author
  return (author, project)
  where

fetchAuthorById :: Pool -> AuthorId -> IO (Either Pool.UsageError (Maybe (Author Result)))
fetchAuthorById pool aid = Pool.use pool $ Session.statement () (Rel8.runMaybe (Rel8.select $ findAuthorById aid))

instance AuthorService DBM where
  getAuthorById :: AuthorId -> DBM (Maybe AuthorView)
  getAuthorById aid = DBM $ do
    pool <- ask
    result <- lift . liftIO $ fetchAuthorById pool aid
    case result of
      Left err -> do
        lift . liftIO $ putStrLn $ "Error querying database: " ++ show err
        return Nothing -- Continue with Nothing on error
      Right maybeAuthor -> return $ fmap authorToAuthorView maybeAuthor
  
  listAuthors :: DBM [AuthorView]
  listAuthors = DBM $ do
    pool <- ask
    result <- lift . liftIO $ Pool.use pool $ Session.statement () (Rel8.run $ Rel8.select $ Rel8.each authorSchema)
    case result of
      Left err -> do
        lift . liftIO $ putStrLn $ "Error querying database: " ++ show err
        return [] -- Continue with empty list on error
      Right authors -> return $ fmap authorToAuthorView authors

authorToAuthorView :: (f ~ Result) => Author f -> AuthorView
authorToAuthorView author = AuthorView
  { authorViewId = authorId author
  , authorViewName = authorName author
  , authorViewUrl = authorUrl author
  }

data ProjectView = ProjectView
  { projectViewAuthorId :: AuthorId
  , projectViewName     :: Text
  }
  deriving stock (Generic)

class Monad m => ProjectService m where
  getProjectById :: AuthorId -> m (Maybe ProjectView)
  listProjects :: m [ProjectView]

instance ProjectService IO where
  getProjectById = undefined
  listProjects = undefined