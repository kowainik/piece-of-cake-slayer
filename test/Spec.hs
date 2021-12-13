module Main (main) where

import Control.Exception (bracket)
import System.IO (hSetEncoding, utf8)
import Test.Hspec (Spec, describe, hspec)

import Piece (mkAppEnv)
import Piece.App.Env (Env (..))
import Piece.App.Monad (AppEnv)
import Piece.Db.Schema (prepareDb)
import Piece.Effects.Log (runAppLogIO_)
import Test.Db (dbSpec)

import qualified Data.Pool as Pool


hspecTests :: AppEnv -> Spec
hspecTests env = describe "Unit tests" $ do
    dbSpec env

main :: IO ()
main = bracket
    mkAppEnv
    (\Env{..} -> Pool.destroyAllResources envDbPool)
    runTests
  where
    runTests :: AppEnv -> IO ()
    runTests env = do
        -- fix terminal encoding
        hSetEncoding stdout utf8
        hSetEncoding stderr utf8

        -- setup DB tables
        runAppLogIO_ env prepareDb

        -- run all tests
        hspec $ hspecTests env
