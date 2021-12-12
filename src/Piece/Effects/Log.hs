-- | Logging action for the project. Currently just logs the output to terminal.

module Piece.Effects.Log
    ( WithLog
    , mainLogAction

    , runAppAsHandler
    , runAppLogIO
    , runAppLogIO_
    ) where

import CakeSlayer.Error (ErrorWithSource (..))
import Colog (LogAction, Message, Msg (..), Severity, filterBySeverity, log, pattern E,
              richMessageAction)
import Control.Monad.Except (liftEither)
import Servant.Server (Handler)

import Piece.App.Error (AppError, toHttpError)
import Piece.App.Monad (App, AppEnv, runAppAsIO)

import qualified Colog


-- | 'Colog.WithLog' alias specialized to 'Message' data type.
type WithLog env m = Colog.WithLog env Message m

-- | Maing log action for the application. Prints message with some metadata to @stdout@.
mainLogAction :: MonadIO m => Severity -> LogAction m Message
mainLogAction severity =
    filterBySeverity severity msgSeverity richMessageAction

----------------------------------------------------------------------------
-- Application runners with runners
----------------------------------------------------------------------------

-- | Runs application as servant 'Handler'.
runAppAsHandler :: AppEnv -> App a -> Handler a
runAppAsHandler env app = do
    res <- liftIO $ runAppLogIO env app
    liftEither $ first toHttpError res

-- | Runs application like 'runAppAsIO' but also logs error.
runAppLogIO :: AppEnv -> App a -> IO (Either (ErrorWithSource AppError) a)
runAppLogIO env app = do
    appRes <- runAppAsIO env app
    logRes <- whenLeft (Right ()) appRes (\ErrorWithSource{..} -> logAppErrorIO env errorWithSourceType)
    pure $ appRes <* logRes

-- | Like 'runAppAsIO' but discards result.
runAppLogIO_ :: AppEnv -> App a -> IO ()
runAppLogIO_ env app = void $ runAppLogIO env app

----------------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------------

logAppErrorIO :: AppEnv -> AppError -> IO (Either (ErrorWithSource AppError) ())
logAppErrorIO env err = runAppAsIO env $ log E $ show err
