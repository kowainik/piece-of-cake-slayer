module Test.Db.Item
    ( itemSpec
    ) where

import Test.Hspec (Spec, describe, it)

import Piece.App.Monad (AppEnv)
import Piece.Core.Id (Id (..))
import Piece.Core.Item (Item (..))
import Piece.Core.WithId (WithId (..))
import Piece.Db.Item (createItem, deleteItem, getItems)

import Test.Assert (equals)


itemSpec :: AppEnv -> Spec
itemSpec env = describe "Item" $ do
    it "Gets all items" $
        env & getItems `equals` defaultItems

    it "Creates new item" $ do
        env & createItem (Item "foo") `equals` Id 3
        env & getItems `equals` (defaultItems ++ [WithId (Id 3) (Item "foo")])

    it "Deletes item" $ do
        let action = deleteItem (Id 3) >> getItems
        env & action `equals` defaultItems

defaultItems :: [WithId Item]
defaultItems =
    [ WithId (Id 1) (Item "test item 1")
    , WithId (Id 2) (Item "test item 2")
    ]
