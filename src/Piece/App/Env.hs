module Piece.App.Env
    ( Env (..)
    ) where

import Colog (HasLog (..), LogAction, Message)


data Env (m :: Type -> Type) = Env
    { envLogAction :: LogAction m Message
    }

instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = envLogAction

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newAction env = env { envLogAction = newAction }
