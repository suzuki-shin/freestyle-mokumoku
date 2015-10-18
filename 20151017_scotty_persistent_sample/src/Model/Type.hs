{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Model.Type
    ( Skill(..)
      ) where

import qualified Data.Aeson              as A
import           Data.Int                (Int64)
import qualified Data.Map.Strict         as M
import           Data.Text.Lazy          (Text)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           GHC.Generics

data Skill = Haskell | Ruby | PHP | Swift deriving (Show, Read, Eq, Ord, Generic)
derivePersistField "Skill"

instance A.FromJSON Skill
instance A.ToJSON Skill
