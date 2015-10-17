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

module Lib
    ( run
    ) where

import           Control.Applicative                   ((<$>))
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Logger                  (NoLoggingT)
import           Control.Monad.Trans.Control           (MonadBaseControl)
import           Control.Monad.Trans.Resource.Internal (ResourceT)
import qualified Data.Aeson                            as A
import           Data.Foldable                         (foldl')
import qualified Data.Map.Strict                       as M
import           Data.Maybe                            (fromJust)
import           Data.Text.Lazy                        (Text, append)
import qualified Data.Text.Lazy
import qualified Database.Persist                      as P
import qualified Database.Persist.Sqlite               as P
import           Database.Persist.TH
import           GHC.Generics
import           GHC.Int                               (Int64)
import qualified Network.Wai.Middleware.RequestLogger  as L
import qualified Web.Scotty                            as S


{- |
Model
-}
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  age Int
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


{- |
Controller
-}
run :: IO ()
run = S.scotty 3000 $ do
  S.middleware L.logStdoutDev

  S.get "/user/:id" $ do
    userId <- S.param "id"
    user <- getUser userId
    S.json user

  -- curl -v -H "Accept: application/json" -H "Content-type: application/json" -X POST -d '{"userName":"hoge", "userAge":20}' localhost:3000
  S.post "/user" $ do
    u <- S.jsonData :: S.ActionM User
    userId <- insertUser u
    S.json userId

  S.notFound $
    S.text "there is no such route."

