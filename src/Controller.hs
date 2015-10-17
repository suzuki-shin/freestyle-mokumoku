{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Controller
    ( run
    ) where

import           Control.Applicative                   ((<$>))
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Logger                  (NoLoggingT)
import           Control.Monad.Trans.Control           (MonadBaseControl)
import           Control.Monad.Trans.Resource.Internal (ResourceT)
import qualified Data.Aeson                            as A
import           Data.Foldable                         (foldl')
import           Model
import qualified Network.Wai.Middleware.RequestLogger  as L
import qualified Web.Scotty                            as S

run :: IO ()
run = S.scotty 3000 $ do
  S.middleware L.logStdoutDev

  S.get "/user/:id" $ do
    userId <- S.param "id"
    user <- getUser userId
    S.json user

  -- curl -v -H "Accept: application/json" -H "Content-type: application/json" -X POST -d '{"userName":"hoge", "userAge":20, "userSkills":[]}' localhost:3000/user
  S.post "/user" $ do
    u <- S.jsonData :: S.ActionM User
    userId <- insertUser u
    S.json userId

  S.notFound $
    S.text "there is no such route."

