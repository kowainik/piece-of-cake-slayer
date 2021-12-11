{- | Application error type.
-}

module Piece.App.Error
    ( WithError
    , AppError (..)
    ) where

import Network.HTTP.Types.Header (HeaderName)

import qualified CakeSlayer.Error as CakeSlayer


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
    deriving stock (Show, Eq)
