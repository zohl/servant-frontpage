{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Servant.Utils.FrontPage (
    FrontPageEndpoint
  , FrontPage
  , mkFrontPage
  ) where

import Control.Arrow ((&&&))
import Control.Monad (liftM2)
import Control.Monad.Catch (handleJust)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Time.Clock (UTCTime(..))
import Data.Typeable (Typeable)
import Servant (Server, Get, throwError, err404)
import Servant.API (Accept (..), MimeRender (..))
import System.Directory (getModificationTime)
import System.IO.Error (isDoesNotExistError)
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Media as M


data HTML deriving Typeable

instance Accept HTML where
  contentType _ = "text" M.// "html" M./: ("charset", "utf-8")

instance MimeRender HTML BSL.ByteString where
  mimeRender _ = id


type FrontPageEndpoint = Get '[HTML] BSL.ByteString
type FrontPage = Server FrontPageEndpoint

data FrontPageState = FrontPageState {
    fpsModificationTime :: UTCTime
  , fpsContents         :: BSL.ByteString
  }

mkFrontPageState :: FilePath -> IO FrontPageState
mkFrontPageState path = (liftM2 FrontPageState) (getModificationTime path) (BSL.readFile path)

frontPage :: FilePath -> IORef FrontPageState -> FrontPage
frontPage path refState = handleJust
  (\e -> if isDoesNotExistError e then Just () else Nothing )
  (\_ -> throwError err404)
  (liftIO $ do
    modificationTime <- getModificationTime path
    state <- readIORef refState
    case (modificationTime > fpsModificationTime state) of
      False -> return $ fpsContents state
      True  -> do
        state' <- mkFrontPageState path
        atomicModifyIORef' refState $ \state'' -> id &&& fpsContents $
          if (fpsModificationTime state == fpsModificationTime state'')
          then state'
          else state'')

mkFrontPage :: FilePath -> IO FrontPage
mkFrontPage path = do
  refState <- newIORef FrontPageState {
      fpsModificationTime = UTCTime (toEnum 0) 0
    , fpsContents = ""
    }
  return $ frontPage path refState
