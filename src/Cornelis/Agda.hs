{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Cornelis.Agda where

import           Control.Concurrent.Chan.Unagi (newChan, readChan, writeChan)
import           Control.Lens
import           Control.Monad (forever, replicateM_)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Cornelis.Debug (reportExceptions, debugString)
import           Cornelis.InfoWin (buildInfoBuffer)
import           Cornelis.Types
import           Cornelis.Types.Agda
import           Cornelis.Utils
import           Data.Aeson
import           Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy.IO as LT
import           Neovim hiding (err)
import           Neovim.API.Text
import           System.IO hiding (hGetLine)
import           System.Process


------------------------------------------------------------------------------
-- | When true, dump out received JSON as it arrives.
debugJson :: Neovim CornelisEnv Bool
debugJson = asks $ cc_debug . ce_config

------------------------------------------------------------------------------
-- | Create an 'Agda' environment for the given buffer. This spawns an
-- asynchronous thread that keeps an agda process alive as long as vim is open.
--
-- TODO(sandy): This leaks the process when the buffer is closed.
spawnAgda :: Buffer -> Neovim CornelisEnv Agda
spawnAgda buffer = do
  (m_in, m_out, _, hdl) <-
    liftIO $ createProcess $
      (proc "agda" ["--interaction-json"])
        { std_in = CreatePipe , std_out = CreatePipe }
  (c_in, c_out) <- liftIO newChan
  ready <- liftIO $ newIORef True
  case (m_in, m_out) of
    (Just hin, Just hout) -> do
      liftIO $ do
        hSetBuffering hin NoBuffering
        hSetBuffering hout NoBuffering

      let whenReady act = liftIO $ do
            -- Agda outputs "JSON> " when it is ready
            -- We skip it, and set the ready flag
            c <- hLookAhead hout
            case c of
              'J' -> replicateM_ 6 (hGetChar hout) >> act
              _ -> pure ()

      -- On the first load, we make ourselves ready before Agda tells us anything
      void $ neovimAsync $ (whenReady (pure ()) >>) . forever $ reportExceptions $ do
        whenReady $ atomicWriteIORef ready True
        resp <- liftIO $ LT.hGetLine hout
        chan <- asks ce_stream
        case eitherDecode @Response $ encodeUtf8 resp of
          _ | LT.null resp -> pure ()
          Left err -> vim_report_error $ T.pack err
          Right res -> do
            case res of
              HighlightingInfo _ _ -> pure ()
              _ -> whenM debugJson $ debugString $ LT.unpack resp
            liftIO $ writeChan chan $ AgdaResp buffer res

      void $ neovimAsync $ liftIO $ forever $ do
        msg <- readChan c_out
        atomicWriteIORef ready False
        hPutStrLn hin msg

      pure $ Agda buffer ready c_in hdl
    (_, _) -> error "can't start agda"


------------------------------------------------------------------------------
-- | Drop a prefix from the text, if it exists.
dropPrefix :: LT.Text -> LT.Text -> LT.Text
dropPrefix pref msg
  | LT.isPrefixOf pref msg = LT.drop (LT.length pref) msg
  | otherwise = msg


------------------------------------------------------------------------------
-- | Send an 'Interaction' to an 'Agda'.
runIOTCM :: Interaction -> Agda -> Neovim env ()
runIOTCM i agda = do
  iotcm <- buildIOTCM i $ a_buffer agda
  -- liftIO $ appendFile "/tmp/agda.spy" $ show iotcm
  liftIO $ writeChan (a_req agda) (show iotcm)


------------------------------------------------------------------------------
-- | Construct an 'IOTCM' for a buffer.
buildIOTCM :: Interaction -> Buffer -> Neovim env IOTCM
buildIOTCM i buffer = do
  fp <- buffer_get_name buffer
  pure $ IOTCM fp NonInteractive Direct i


------------------------------------------------------------------------------
-- | Get the current buffer and run the continuation.
withCurrentBuffer :: (Buffer -> Neovim env a) -> Neovim env a
withCurrentBuffer f = vim_get_current_buffer >>= f


------------------------------------------------------------------------------
-- | Ensure we have a 'BufferStuff' attached to the current buffer.
withAgda :: Neovim CornelisEnv a -> Neovim CornelisEnv a
withAgda m = do
  buffer <- vim_get_current_buffer
  gets (M.lookup buffer . cs_buffers) >>= \case
    Just _ -> m
    Nothing -> do
      agda <- spawnAgda buffer
      iw <- buildInfoBuffer
      modify' $ #cs_buffers %~ M.insert buffer BufferStuff
        { bs_agda_proc = agda
        , bs_ips = mempty
        , bs_ip_exts = mempty
        , bs_goto_sites = mempty
        , bs_goals = AllGoalsWarnings [] [] [] []
        , bs_info_win = iw
        , bs_code_map = mempty
        }
      m


------------------------------------------------------------------------------
-- | Get the 'Agda' environment for a given buffer.
getAgda :: Buffer -> Neovim CornelisEnv Agda
getAgda buffer = gets $ bs_agda_proc . (M.! buffer) . cs_buffers

