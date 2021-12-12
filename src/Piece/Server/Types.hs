-- | This module introduce aliases to use for @servant-generic@ types and functions writing.

module Piece.Server.Types
    ( AppServer
    , ToApi
    ) where

import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (AsServerT)

import Piece.App.Monad (App)


type AppServer = AsServerT App
type ToApi (site :: Type -> Type) = ToServantApi site
