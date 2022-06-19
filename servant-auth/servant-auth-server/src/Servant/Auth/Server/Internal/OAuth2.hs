{-# LANGUAGE CPP #-}
module Servant.Auth.Server.Internal.OAuth2 where

import           Blaze.ByteString.Builder (toByteString)
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Crypto.JOSE              as Jose
import qualified Crypto.JWT               as Jose
import           Data.ByteArray           (constEq)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base64   as BS64
import qualified Data.ByteString.Lazy     as BSL
import           Data.CaseInsensitive     (mk)
import           Data.Maybe               (fromMaybe)
import           Data.Time.Calendar       (Day(..))
import           Data.Time.Clock          (UTCTime(..), secondsToDiffTime)
import           Network.HTTP.Types       (methodGet)
import           Network.HTTP.Types.Header(hCookie)
import           Network.Wai              (Request, requestHeaders, requestMethod)
import           Servant                  (AddHeader, addHeader)
import           System.Entropy           (getEntropy)
import           Web.Cookie

import qualified Network.Wai.Middleware.Auth          as Wai
import qualified Network.Wai.Middleware.Auth.Provider as Wai

import Servant.Auth.Server.Internal.ConfigTypes
import Servant.Auth.Server.Internal.Types

data OAuth2Settings = OAuth2Settings
  { clientId :: Int
  , clientSecret :: Int
  }

oauth2AuthCheck :: OAuth2Settings -> Wai.Provider -> AuthCheck usr
oauth2AuthCheck settings provider = do
  request <- ask
  let suffix = ["login", "complete"]
      a = undefined
      success authLoginState = undefined -- pure authLoginState
      failure = undefined
  liftIO $ Wai.handleLogin provider request suffix a success failure
  undefined
