{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cornelis.Utils where

import           Control.Concurrent.Async
import           Control.Exception (throwIO)
import           Control.Lens ((%~))
import           Control.Monad.IO.Unlift (MonadUnliftIO(withRunInIO))
import           Control.Monad.Reader (withReaderT)
import           Control.Monad.State.Class
import           Cornelis.Types
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text.Encoding (decodeUtf8)
import           Data.Traversable
import qualified Data.Vector as V
import           Neovim hiding (err)
import           Neovim.API.Text
import           Neovim.Context.Internal (Neovim(..), retypeConfig)

objectToInt :: Num a => Object -> Maybe a
objectToInt (ObjectUInt w) = Just $ fromIntegral w
objectToInt (ObjectInt w) = Just $ fromIntegral w
objectToInt _ = Nothing

objectToText :: Object -> Maybe Text
objectToText (ObjectString w) = Just $ decodeUtf8 w
objectToText _ = Nothing

objectToBool :: Object -> Maybe Bool
objectToBool (ObjectBool b) = Just b
objectToBool o = objectToInt @Int o >>= \case
    0 -> Just False
    1 -> Just True
    _ -> Nothing

neovimAsync :: (MonadUnliftIO m) => m a -> m (Async a)
neovimAsync m =
  withRunInIO $ \lower ->
    liftIO $ async $ lower m

savingCurrentPosition :: Window -> Neovim env a -> Neovim env a
savingCurrentPosition w m = do
  c <- window_get_cursor w
  m <* window_set_cursor w c

savingCurrentWindow :: Neovim env a -> Neovim env a
savingCurrentWindow m = do
  w <- nvim_get_current_win
  m <* nvim_set_current_win w

windowsForBuffer :: Buffer -> Neovim env [Window]
windowsForBuffer b = do
  wins <- fmap V.toList vim_get_windows
  fmap catMaybes $ for wins $ \w -> do
    wb <- window_get_buffer w
    pure $ case wb == b of
      False -> Nothing
      True -> Just w

visibleBuffers :: Neovim env [(Window, Buffer)]
visibleBuffers = do
  wins <- fmap V.toList vim_get_windows
  for wins $ \w -> fmap (w, ) $ window_get_buffer w

criticalFailure :: Text -> Neovim env a
criticalFailure err = do
  vim_report_error err
  liftIO $ throwIO $ ErrorResult "critical error" ObjectNil

modifyBufferStuff :: Buffer -> (BufferStuff -> BufferStuff) -> Neovim CornelisEnv ()
modifyBufferStuff b f = modify' $! #cs_buffers %~ M.update (Just . f) b

withBufferStuff :: Monoid a => Buffer -> (BufferStuff -> Neovim CornelisEnv a) -> Neovim CornelisEnv a
withBufferStuff b f =
  gets (M.lookup b . cs_buffers) >>= \case
    Nothing -> vim_report_error "no buffer stuff!" >> pure mempty
    Just bs -> f bs

withLocalEnv :: env -> Neovim env a -> Neovim env' a
withLocalEnv env (Neovim t) = Neovim $ flip withReaderT t $ retypeConfig env
