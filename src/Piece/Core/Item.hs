module Piece.Core.Item
    ( Item (..)
    ) where

import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToRow (ToRow)


newtype Item = Item
    { itemText :: Text
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (FromRow, ToRow)
