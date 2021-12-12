{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @items@ table.

module Piece.Db.Item
       ( getItems
       , createItem
       , deleteItem
       ) where

import CakeSlayer.Db (WithDb, asSingleRowWith, executeNamed_, queryRaw, returning)
import CakeSlayer.Error (ErrorWithSource)
import Control.Monad.Except (liftEither)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import PgNamed (PgNamedError, (=?))
import Relude.Extra.Bifunctor (firstF)

import Piece.App.Error (AppError (..), WithError)
import Piece.Core.Id (Id)
import Piece.Core.Item (Item)
import Piece.Core.WithId (WithId)


-- | Returns all items in the database.
getItems :: (WithDb env m) => m [WithId Item]
getItems = queryRaw [sql|
    SELECT id, text
    FROM items
|]

createItem :: (WithDb env m, WithError m) => Item -> m (Id Item)
createItem item = fmap fromOnly $ asSingleRowWith err $ returning [sql|
    INSERT INTO items
        (text)
    VALUES
        (?)
    RETURNING id
|] [ item ]
  where
    err :: AppError
    err = DbError "Expecting 'id' after inserting Item to 'items'"

deleteItem :: (WithDb env m, WithError m) => Id Item -> m ()
deleteItem itemId = fromPgNamedError $ executeNamed_ [sql|
    DELETE FROM items
    WHERE id = ?id
|] [ "id" =? itemId
   ]

fromPgNamedError :: WithError m => ExceptT (ErrorWithSource PgNamedError) m a -> m a
fromPgNamedError action =
    firstF (fmap DbNamedError) (runExceptT action) >>= liftEither
