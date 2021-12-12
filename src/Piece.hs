module Piece
    ( mkAppEnv
    , runServer
    , main
    ) where

import CakeSlayer.Db (initialisePool)
import Colog (Severity (Debug))
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)

import Piece.App.Env (Env (..))
import Piece.App.Monad (AppEnv, runApp)
import Piece.Db.Schema (prepareDb)
import Piece.Effects.Log (mainLogAction)
import Piece.Server (PieceApi, server)


mkAppEnv :: IO AppEnv
mkAppEnv = do
    -- IO configuration
    envDbPool <- initialisePool
        "host=localhost port=5432 user=postgres password=postgres dbname=postgres"

    -- pure configuration
    let envLogAction = mainLogAction Debug
    pure Env{..}

runServer :: AppEnv -> IO ()
runServer env = runApp env prepareDb >> run 8080 application
  where
    application = serve (Proxy @PieceApi) (server env)

main :: IO ()
main = mkAppEnv >>= runServer
