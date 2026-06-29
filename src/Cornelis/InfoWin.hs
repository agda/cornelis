{-# LANGUAGE OverloadedStrings #-}

module Cornelis.InfoWin (closeInfoWindows, showInfoWindow, buildInfoBuffer) where

import           Prelude hiding (Left, Right)
import           Control.Monad (unless)
import           Control.Monad.State.Class
import           Cornelis.Pretty
import           Cornelis.Types
import           Cornelis.Utils (withBufferStuff, windowsForBuffer, savingCurrentWindow, visibleBuffers)
import qualified Cornelis.Vim.Compat as Compat
import           Data.Foldable (for_)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Traversable (for)
import qualified Data.Vector as V
import           Neovim
import           Neovim.API.Text
import           Prettyprinter (layoutPretty, LayoutOptions (LayoutOptions), PageWidth (AvailablePerLine))
import           Prettyprinter.Render.Text (renderStrict)


cornelisWindowVar :: Text
cornelisWindowVar = "cornelis_window"

getBufferVariableOfWindow :: NvimObject o => Text -> Window -> Neovim env (Maybe o)
getBufferVariableOfWindow variableName window = do
  window_is_valid window >>= \case
    False -> pure Nothing
    True -> do
      buffer <- window_get_buffer window
      let getVariableValue = Just . fromObjectUnsafe <$> buffer_get_var buffer variableName
      getVariableValue `catchNeovimException` const (pure Nothing)

closeInfoWindowsForUnseenBuffers :: Neovim CornelisEnv ()
closeInfoWindowsForUnseenBuffers = do
  seen <- S.fromList . fmap snd <$> visibleBuffers
  bufs <- gets cs_buffers
  let known = M.keysSet bufs
      unseen = known S.\\ seen
  for_ unseen $ \b -> do
    for_ (M.lookup b bufs) $ \bs -> do
      ws <- windowsForBuffer $ iw_buffer $ bs_info_win bs
      for_ ws $ flip nvim_win_close True

closeInfoWindows :: Neovim env ()
closeInfoWindows = do
  ws <- Compat.nvim_list_wins
  for_ ws $ \w -> getBufferVariableOfWindow cornelisWindowVar w >>= \case
    Just True -> nvim_win_close w True
    _ -> pure ()


closeInfoWindowForBuffer :: BufferStuff -> Neovim CornelisEnv ()
closeInfoWindowForBuffer bs = do
  let InfoBuffer ib = bs_info_win bs
  ws <- windowsForBuffer ib
  for_ ws $ flip nvim_win_close True



showInfoWindow :: Buffer -> Doc HighlightGroup -> Neovim CornelisEnv ()
showInfoWindow b doc = withBufferStuff b $ \bs -> do
  let ib = bs_info_win bs
  closeInfoWindowsForUnseenBuffers

  vis <- visibleBuffers
  ns <- asks ce_namespace

  -- Check if the info win still exists, and if so, just modify it
  found <- fmap or $
    for vis $ \(w, vb) -> do
      case vb == iw_buffer ib of
        False -> pure False
        True -> do
          writeInfoBuffer ns ib doc
          resizeInfoWin w ib
          pure True

  -- Otherwise we need to rebuild it
  unless found $ do
    closeInfoWindowForBuffer bs
    writeInfoBuffer ns ib doc
    ws <- windowsForBuffer b
    for_ ws $ buildInfoWindow ib



buildInfoBuffer :: Neovim env InfoBuffer
buildInfoBuffer = do
  b <-
    -- not listed in the buffer list, is throwaway
    nvim_create_buf False True

  -- Setup things in the buffer
  void $ buffer_set_var b cornelisWindowVar $ ObjectBool True
  nvim_buf_set_option b "modifiable" $ ObjectBool False
  nvim_buf_set_option b "filetype" $ ObjectString "agdainfo"
  pure $ InfoBuffer b




buildInfoWindow :: InfoBuffer -> Window -> Neovim CornelisEnv Window
buildInfoWindow (InfoBuffer split_buf) w = savingCurrentWindow $ do
  nvim_set_current_win w
  max_height <-  asks $ T.pack . show . cc_max_height . ce_config
  max_width <- asks $ T.pack . show . cc_max_width . ce_config
  asks (cc_split_location . ce_config) >>= \case
    Vertical   -> vim_command $ max_width <> " vsplit"
    Horizontal -> vim_command $ max_height <> " split"
    OnLeft     -> vim_command $ "topleft " <> max_width <> " vsplit"
    OnRight    -> vim_command $ "botright " <> max_width <> " vsplit"
    OnTop      -> vim_command $ "topleft " <> max_height <> " split"
    OnBottom   -> vim_command $ "botright " <> max_height <> " split"
  split_win <- nvim_get_current_win
  nvim_win_set_buf split_win split_buf

  -- Setup things in the window
  nvim_win_set_option split_win "relativenumber" $ ObjectBool False
  nvim_win_set_option split_win "number" $ ObjectBool False

  resizeInfoWin split_win (InfoBuffer split_buf)

  pure split_win

resizeInfoWin :: Window -> InfoBuffer -> Neovim CornelisEnv ()
resizeInfoWin w ib = do
  t <- nvim_buf_get_lines (iw_buffer ib) 0 (-1) False
  max_size <- asks $ cc_max_height . ce_config
  let size = min max_size $ fromIntegral $ V.length t
  asks (cc_split_location . ce_config) >>= \case
    Vertical   -> pure ()
    Horizontal -> window_set_height w size
    OnLeft     -> pure ()
    OnRight    -> pure ()
    OnTop      -> window_set_height w size
    OnBottom   -> window_set_height w size



writeInfoBuffer :: Int64 -> InfoBuffer -> Doc HighlightGroup -> Neovim env ()
writeInfoBuffer ns iw doc = do
  -- TODO(sandy): Bad choice for a window, but good enough?
  w <- nvim_get_current_win
  width <- window_get_width w

  let sds = layoutPretty (LayoutOptions (AvailablePerLine (fromIntegral width) 0.8)) doc
      (hls, sds') = renderWithHlGroups sds
      s = T.lines $ renderStrict sds'

  let b = iw_buffer iw
  nvim_buf_set_option b "modifiable" $ ObjectBool True
  buffer_set_lines b 0 (-1) True $ toObject <$> V.fromList s

  for_ (concatMap spanInfoHighlights hls) $ \(InfoHighlight (l, sc) ec hg) ->
    nvim_buf_add_highlight
      b ns
      (T.pack $ show hg)
      l sc ec
  nvim_buf_set_option b "modifiable" $ ObjectBool False

