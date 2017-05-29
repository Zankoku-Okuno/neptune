module Neptune.Error where

import ClassyPrelude
import Neptune.Core

import qualified Network.HTTP.Types as Http -- FIXME don't rely on http explicitly


type ErrorReporter = Request -> Error -> Response


haikuErrorResponse :: Request -> Error -> Response
haikuErrorResponse req BadResource =
    Response { status = Http.status404, responseBody = Just ("text/plain", "Error 404:\nYour file could not be found,\nTry again later.") }
haikuErrorResponse req (BadVerb allowed) = -- FIXME include the allowed methods
    Response { status = Http.status405, responseBody = Just ("text/plain", "Error 405:\nWhat are you trying to do?\nMethod not allowed.")}
haikuErrorResponse req (BadMedia acceptable) = --FIXME include the acceptable media types
    Response { status = Http.status406, responseBody = Just ("text/plain", "Error 406:\nUnknown format requested,\nNot acceptable.") }


fallbackErrorResponse :: SomeException -> Response
fallbackErrorResponse e = Response
    { status = Http.status500
    , responseBody = Just ("text/plain", encodeUtf8 $ tlshow e)
    }