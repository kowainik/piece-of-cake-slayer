{- | Application error type.
-}

module Piece.App.Error
    ( WithError
    , AppError (..)
    , toHttpError
    ) where

import Data.CaseInsensitive (foldedCase)
import Network.HTTP.Types.Header (HeaderName)
import PgNamed (PgNamedError)
import Servant.Server (err401, err404, err417, err500, errBody)

import qualified CakeSlayer.Error as CakeSlayer
import qualified Servant.Server as Servant (ServerError)


type WithError m = CakeSlayer.WithError AppError m

-- | App errors type.
data AppError
    {- | General not found. -}
    = NotFound

    {- | Some exceptional circumstance has happened to stop execution and return.
    Optional text to provide some context in server logs.
    -}
    | ServerError Text

    {- | A required permission level was not met. Optional text to provide some context. -}
    | NotAllowed Text

    {- | Given inputs do not conform to the expected format or shape. Optional
    text to provide some context in server logs.
    -}
    | Invalid Text

    {- | Some header expected, but not present in header list.
    -}
    | MissingHeader HeaderName

    {- | An authentication header that was required was provided but not in a
    format that the server can understand.
    -}
    | HeaderDecodeError Text

    {- | Data base specific errors. -}
    | DbError Text

    {- | Data base named parameters errors. -}
    | DbNamedError PgNamedError
    deriving stock (Show, Eq)

-- | Map 'AppError' into a HTTP error code.
toHttpError :: CakeSlayer.ErrorWithSource AppError -> Servant.ServerError
toHttpError CakeSlayer.ErrorWithSource{..} = case errorWithSourceType of
    NotFound               -> err404
    ServerError msg        -> err500 { errBody = encodeUtf8 msg }
    NotAllowed msg         -> err401 { errBody = encodeUtf8 msg }
    Invalid msg            -> err417 { errBody = encodeUtf8 msg }
    MissingHeader name     -> err401 { errBody = toLazy $ "Header not found: " <> foldedCase name }
    HeaderDecodeError name -> err401 { errBody = encodeUtf8 $ "Unable to decode header: " <> name }
    DbError e              -> err500 { errBody = encodeUtf8 e }
    DbNamedError e         -> err500 { errBody = show e }
