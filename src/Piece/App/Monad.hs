module Piece.App.Monad
    ( App (..)
    , AppEnv
    , runApp
    , runAppAsIO
    ) where

import CakeSlayer (ErrorWithSource)
import Control.Monad.Except (MonadError)
import UnliftIO (MonadUnliftIO)

import Piece.App.Env (Env)
import Piece.App.Error (AppError)

import qualified CakeSlayer.Monad as CakeSlayer


-- | 'Env' data type parameterized by 'App' monad
type AppEnv = Env App

-- | Main application monad.
newtype App a = App
    { unApp :: CakeSlayer.App AppError AppEnv a
    } deriving newtype
        ( Functor, Applicative, Monad, MonadIO, MonadUnliftIO
        , MonadReader AppEnv, MonadError (ErrorWithSource AppError)
        )

{- | Run application by providing environment.

Throws 'AppException' if application has unhandled 'throwError'. Use
'runAppAsIO' to handle exceptions as well.
-}
runApp :: AppEnv -> App a -> IO a
runApp env = CakeSlayer.runApp env . unApp

{- | Like 'runApp' but also catches 'AppException' and unwraps 'ErrorWithSource'
from it. Use this function to handle errors outside 'App' monad.
-}
runAppAsIO :: AppEnv -> App a -> IO (Either (ErrorWithSource AppError) a)
runAppAsIO env = CakeSlayer.runAppAsIO env . unApp
