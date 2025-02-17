{-# LANGUAGE OverloadedStrings #-}

module Cornelis.Config where

import           Cornelis.Types
import           Cornelis.Types.Agda (Rewrite(..))
import           Cornelis.Utils (objectToInt, objectToText)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Neovim
import           Neovim.API.Text
import           Control.Monad ((<=<))


------------------------------------------------------------------------------
-- | Attempt to get a variable from vim.
getVar :: Text -> Neovim env (Maybe Object)
getVar v
  = catchNeovimException (Just <$> vim_get_var v)
  $ const
  $ pure Nothing


------------------------------------------------------------------------------
-- | Get the first variable from vim that succeeds. Useful for variable names
-- that have changed over time.
getVarWithAlternatives :: [Text] -> Neovim env (Maybe Object)
getVarWithAlternatives = fmap getFirst . foldMap (fmap First . getVar)


------------------------------------------------------------------------------
-- | Build a 'CornelisConfig' from .vimrc
getConfig :: Neovim env CornelisConfig
getConfig
  = CornelisConfig
    <$> fmap (fromMaybe 31 . (objectToInt =<<))
        (getVar "cornelis_max_size")
    <*> fmap (fromMaybe 31 . (objectToInt =<<))
        (getVar "cornelis_max_width")
    <*> (fromMaybe Horizontal . (>>= (readSplitLocation . T.unpack <=< objectToText)) <$>
        getVarWithAlternatives ["cornelis_split_location", "cornelis_split_direction"])
    <*> (fromMaybe HeadNormal . (>>= (readRewrite . T.unpack <=< objectToText)) <$>
        (getVar "cornelis_rewrite_mode"))

