{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Piece.Server.Item
    ( -- * API
      ItemsApi
    , itemsServer

      -- * Handlers
    , getItemsHandler
    ) where

import CakeSlayer.Db (WithDb)
import Colog (log, pattern D, pattern I)
import Servant (Get, JSON, Post, ReqBody, (:>))
import Servant.API.Generic ((:-))

import Piece.App.Error (WithError)
import Piece.Core.Id (Id)
import Piece.Core.Item (Item)
import Piece.Core.WithId (WithId (..))
import Piece.Db.Item (createItem, deleteItem, getItems)
import Piece.Effects.Log (WithLog)
import Piece.Server.Types (AppServer, ToApi)


type ItemsApi = ToApi ItemsSite

data ItemsSite route = ItemsSite
    { getItemsRoute :: route
        :- "items"
        :> Get '[JSON] [WithId Item]

    , createItemRoute :: route
        :- "createItem"
        :> ReqBody '[JSON] Item
        :> Post '[JSON] (Id Item)

    , deleteItemRoute :: route
        :- "deleteItem"
        :> ReqBody '[JSON] (Id Item)
        :> Post '[JSON] ()
    } deriving stock (Generic)

itemsServer :: ItemsSite AppServer
itemsServer = ItemsSite
    { getItemsRoute = getItemsHandler
    , createItemRoute = createItemHandler
    , deleteItemRoute = deleteItemHandler
    }

getItemsHandler
    :: ( WithDb env m
       , WithLog env m
       )
    => m [WithId Item]
getItemsHandler = do
    log I "Getting all items..."
    items <- getItems
    log D $ "Total items fetched: " <> show (length items)
    pure items

createItemHandler
    :: ( WithDb env m
       , WithLog env m
       , WithError m
       )
    => Item
    -> m (Id Item)
createItemHandler item = do
    log I $ "Creating item: " <> show item
    newItemId <- createItem item
    log D $ "Created item with id: " <> show newItemId
    pure newItemId

deleteItemHandler
    :: ( WithDb env m
       , WithLog env m
       , WithError m
       )
    => Id Item
    -> m ()
deleteItemHandler itemId = do
    log I $ "Deleting item with id: " <> show itemId
    deleteItem itemId
