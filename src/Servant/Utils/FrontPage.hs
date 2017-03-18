{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Servant.Utils.FrontPage (
    FrontPageEndpoint
  , FrontPage
  , mkFrontPage
  , mkFrontPageState
  ) where

import Control.Arrow ((&&&))
import Control.Monad (liftM2)
import Control.Monad.Catch (handleJust)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Time.Clock (UTCTime(..))
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai (Application, Response, responseLBS)
import Servant.API (Raw)
import System.Directory (getModificationTime)
import System.IO.Error (isDoesNotExistError)
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Media as M


type FrontPageEndpoint = Raw
type FrontPage = Application

data FrontPageState = FrontPageState {
    fpsModificationTime :: UTCTime
  , fpsContents         :: BSL.ByteString
  }

mkFrontPageState :: FilePath -> IO FrontPageState
mkFrontPageState path = (liftM2 FrontPageState) (getModificationTime path) (BSL.readFile path)

responseNotFound :: Response
responseNotFound = responseLBS
  status404
  [(hContentType, M.renderHeader $ "text" M.// "plain")]
  "Not found"

responsePage :: BSL.ByteString -> Response
responsePage = responseLBS
  status200
  [(hContentType, M.renderHeader $ "text" M.// "html" M./: ("charset", "utf-8"))]

frontPage :: FilePath -> IORef FrontPageState -> FrontPage
frontPage path refState _req respond = handleJust
  (\e -> if isDoesNotExistError e then Just () else Nothing)
  (\_ -> respond responseNotFound)
    (liftIO $ do
      modificationTime <- getModificationTime path
      state <- readIORef refState
      case (modificationTime > fpsModificationTime state) of
        False -> respond $ responsePage (fpsContents state)
        True  -> do
          state' <- mkFrontPageState path
          contents <- atomicModifyIORef' refState $
            \state'' -> id &&& fpsContents $
              if (fpsModificationTime state == fpsModificationTime state'')
                then state'
                else state''
          respond $ responsePage contents)

mkFrontPage :: FilePath -> IO FrontPage
mkFrontPage path = do
  refState <- newIORef FrontPageState {
      fpsModificationTime = UTCTime (toEnum 0) 0
    , fpsContents = ""
    }
  return $ frontPage path refState

