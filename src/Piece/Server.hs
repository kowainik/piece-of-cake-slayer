{-# LANGUAGE TypeOperators #-}

module Piece.Server
    ( PieceApi
    , server
    ) where

import Servant.API.Generic (toServant, (:-))
import Servant.Server (Server, hoistServer)

import Piece.App.Monad (AppEnv)
import Piece.Effects.Log (runAppAsHandler)
import Piece.Server.Item (ItemsApi, itemsServer)
import Piece.Server.Types (AppServer, ToApi)


newtype PieceSite route = PieceSite
    { pieceItemsRoute :: route :- ItemsApi
    } deriving stock (Generic)

type PieceApi = ToApi PieceSite

pieceServer :: PieceSite AppServer
pieceServer = PieceSite
    { pieceItemsRoute = toServant itemsServer
    }

server :: AppEnv -> Server PieceApi
server env = hoistServer (Proxy @PieceApi) (runAppAsHandler env) (toServant pieceServer)
