module Test.Db
    ( dbSpec
    ) where

import Test.Hspec (Spec, describe)

import Piece.App.Monad (AppEnv)

import Test.Db.Item (itemSpec)


dbSpec :: AppEnv -> Spec
dbSpec env = describe "Database tests" $ do
    itemSpec env
