{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Model
    ( insertUser
    , getUser
    , User(..)
    ) where

import           Control.Applicative                   ((<$>))
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Logger                  (NoLoggingT)
import           Control.Monad.Trans.Control           (MonadBaseControl)
import           Control.Monad.Trans.Resource.Internal (ResourceT)
import qualified Data.Aeson                            as A
import           Data.Text.Lazy                        (Text)
import qualified Database.Persist                      as P
import qualified Database.Persist.Sqlite               as P
import           Database.Persist.TH
import           GHC.Generics
import           GHC.Int                               (Int64)
import           Model.Type


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  age Int
  skills [Skill]
  UniqueUser name
  deriving Show Generic
|]

instance A.FromJSON User
instance A.ToJSON User

runDB :: MonadIO m => P.SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runDB query = liftIO $ P.runSqlite "db.sqlite" $ do
  P.runMigration migrateAll
  query


insertUser :: MonadIO m => User -> m UserId
insertUser k = runDB $ P.insert k


getUser :: MonadIO m => Int64 -> m (Maybe User)
getUser kid = runDB $ P.get (P.toSqlKey kid :: UserId)
