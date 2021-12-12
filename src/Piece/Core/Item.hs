module Piece.Core.Item
    ( Item (..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Elm (Elm, ElmStreet (..))


newtype Item = Item
    { itemText :: Text
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (FromRow, ToRow)
      deriving (Elm, ToJSON, FromJSON) via ElmStreet Item
