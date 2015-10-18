{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Model
    ( insertUser -- ここで
    , getUser    -- 記述されている
    , insertChat -- 関数や型が
    , selectChat -- エクスポートされます。
    , User(..)   -- 逆にここに書かれていないものは
    , Chat(..)   -- このモジュールの外では参照できません。
    ) where

import           Control.Applicative                   ((<$>))
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Logger                  (NoLoggingT)
import           Control.Monad.Trans.Control           (MonadBaseControl)
import           Control.Monad.Trans.Resource.Internal (ResourceT)
import qualified Data.Aeson                            as A
import           Data.Text.Lazy                        (Text)
import           Database.Persist                      ((==.))
import qualified Database.Persist                      as P
import qualified Database.Persist.Sqlite               as P
import           Database.Persist.TH
import           GHC.Int                               (Int64)
import           Model.Type

-- | User型をChat型を定義しています。これらに対応するテーブルが自動で作られます。
-- User型はnameとageとskillsというプロパティを持っていて、それぞれの値はHaskellのText型、Int型、Skill型（Model/Type.hsで定義している）のリストに型付けされています。
-- User型はnameでユニーク制約がついています。
-- また、型名の隣に'json'と書くことによって、JSONへの自動変換とJSONからの自動変換ができるようになっています。
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  name Text
  age Int
  skills [Skill]
  UniqueUser name
  deriving Show
Chat json
  body Text
  userId UserId
  deriving Show
|]

runDB :: MonadIO m => P.SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runDB query = liftIO $ P.runSqlite "db.sqlite" $ do
  P.runMigration migrateAll
  query

-- | User型の値を受け取りDBにインサートしてUserIdを返す
insertUser :: MonadIO m => User -> m UserId
insertUser = runDB . P.insert

-- | Int64型のUserIdを受け取り、User型の値を返す
getUser :: MonadIO m => Int64 -> m (Maybe User)
getUser uid = runDB $ P.get (P.toSqlKey uid :: UserId)

-- | Chat型の値を受け取りDBにインサートしてChatIdを返す
insertChat :: MonadIO m => Chat -> m ChatId
insertChat = runDB . P.insert

-- | Int64型のUserIdを受け取り、そのUserのChat型の値のリストを返す
selectChat :: MonadIO m => Int64 -> m [Chat]
selectChat uid = do
  chats <- runDB $ P.selectList ([ChatUserId ==. P.toSqlKey uid]::[P.Filter Chat]) []
  return $ map P.entityVal chats
