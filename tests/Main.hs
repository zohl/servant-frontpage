{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Exception.Base (bracket)
import Control.Monad (guard, when)
import Network.Wai (Application)
import Test.Hspec (Spec, hspec, describe, it)
import Test.Hspec.Wai (MatchBody(..), matchBody, shouldRespondWith, liftIO, with, get)
import Servant (Proxy(..))
import Servant.Server (serve)
import Servant.Utils.FrontPage
import System.Directory (getTemporaryDirectory, doesFileExist, removeFile, getModificationTime, setModificationTime)
import System.IO (openTempFile, hClose)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8


main :: IO ()
main = withTempFile (hspec . spec) where

  withTempFile :: (FilePath -> IO ()) -> IO ()
  withTempFile f = getTemporaryDirectory >>= \tmpdir -> bracket
    (createTempFile tmpdir)
    removeTempFile
    f

  createTempFile :: FilePath -> IO FilePath
  createTempFile tmpdir = do
    (path, handle) <- openTempFile tmpdir "index.html"
    hClose handle
    return path

  removeTempFile :: FilePath -> IO ()
  removeTempFile path = doesFileExist path >>= flip when (removeFile path)


spec :: FilePath -> Spec
spec pagePath = with (app pagePath) $ do
  describe "GET /" $ do
    let r = get "/"

    it "outputs contents of the file" $ do
      liftIO $ writeFile pagePath "hello!"
      r `shouldRespondWith` 200 { matchBody = bodyEquals "hello!" }

    it "outputs contents of the modified file" $ do
      liftIO $ writeFile pagePath "hello again!"
      r `shouldRespondWith` 200 { matchBody = bodyEquals "hello again!" }

    it "returns 404 when there is no file" $ do
      liftIO $ removeFile pagePath
      r `shouldRespondWith` 404


    it "doesn't read the file from disk when modification time is still the same" $ do
      timestamp <- liftIO $ do
        writeFile pagePath "hello!"
        getModificationTime pagePath

      r `shouldRespondWith` 200 { matchBody = bodyEquals "hello!" }

      liftIO $ do
        threadDelay 1000000
        writeFile pagePath "hello again!"
        setModificationTime pagePath timestamp

      r `shouldRespondWith` 200 { matchBody = bodyEquals "hello!" }


app :: FilePath -> IO Application
app pagePath = do
  frontPage <- mkFrontPage pagePath
  return $ serve (Proxy :: Proxy FrontPageEndpoint) frontPage

bodyEquals :: BSL.ByteString -> MatchBody
bodyEquals body = MatchBody $ \_ body' ->
  (concat [
      "expected \""
      , BSLC8.unpack body
      , "\" got \""
      , BSLC8.unpack body'
      , "\""]) <$ guard (body /= body')

