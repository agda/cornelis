{-# LANGUAGE OverloadedStrings #-}

module Cornelis.InfoWin (closeInfoWindows, closeInfoWindowForBuffer, disposeInfoBuffer, showInfoWindow, buildInfoBuffer) where

import           Prelude hiding (Left, Right)
import           Control.Monad (unless, when)
import           Control.Monad.State.Class
import           Cornelis.Pretty
import           Cornelis.Types
import           Cornelis.Utils (withBufferStuff, windowsForBuffer, savingCurrentWindow, visibleBuffers)
import           Data.Foldable (asum, for_)
import qualified Data.Map as M
import           Data.Maybe (listToMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Traversable (for)
import qualified Data.Vector as V
import           Neovim
import           Neovim.API.Text
import           Prettyprinter (layoutPretty, LayoutOptions (LayoutOptions), PageWidth (AvailablePerLine))
import           Prettyprinter.Render.Text (renderStrict)


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

-- Close only info windows backed by buffers created by this Cornelis instance.
closeInfoWindows :: Neovim CornelisEnv ()
closeInfoWindows = do
  bufs <- gets cs_buffers
  for_ bufs closeInfoWindowForBuffer


closeInfoWindowForBuffer :: BufferStuff -> Neovim CornelisEnv ()
closeInfoWindowForBuffer bs = do
  let InfoBuffer ib = bs_info_win bs
  ws <- windowsForBuffer ib
  for_ ws $ flip nvim_win_close True


disposeInfoBuffer :: BufferStuff -> Neovim CornelisEnv ()
disposeInfoBuffer bs = do
  closeInfoWindowForBuffer bs
  let InfoBuffer ib = bs_info_win bs
  valid <- nvim_buf_is_valid ib
  when valid $ nvim_buf_delete ib M.empty



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
  maxHeight <- asks $ cc_max_height . ce_config
  maxWidth <- asks $ cc_max_width . ce_config
  orientation <- infoWindowOrientation w
  case orientation of
    Just Horizontal -> window_set_height w $ min maxHeight $ fromIntegral $ V.length t
    Just Vertical -> window_set_width w maxWidth
    _ -> pure ()


infoWindowOrientation :: Window -> Neovim env (Maybe SplitLocation)
infoWindowOrientation w = do
  winNumber <- nvim_win_get_number w
  winId <- vim_call_function "win_getid" $ V.singleton $ toObject winNumber
  layout <- vim_eval "winlayout()"
  pure $ listToMaybe . reverse =<< layoutPath winId layout


layoutPath :: Object -> Object -> Maybe [SplitLocation]
layoutPath target (ObjectArray [ObjectString "leaf", winId])
  | target == winId = Just []
layoutPath target (ObjectArray [ObjectString direction, ObjectArray children]) = do
  orientation <- case direction of
    "col" -> Just Horizontal
    "row" -> Just Vertical
    _ -> Nothing
  (orientation :) <$> asum (layoutPath target <$> children)
layoutPath _ _ = Nothing



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
  buffer_set_lines b 0 (-1) True $ V.fromList s

  for_ (concatMap spanInfoHighlights hls) $ \(InfoHighlight (l, sc) ec hg) ->
    nvim_buf_add_highlight
      b ns
      (T.pack $ show hg)
      l sc ec
  nvim_buf_set_option b "modifiable" $ ObjectBool False
