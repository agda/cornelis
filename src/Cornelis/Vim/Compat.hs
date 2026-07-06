module Cornelis.Vim.Compat where

import Control.Monad.IO.Class (MonadIO)
import Data.Either
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Neovim
import Neovim.API.Text (Buffer, Window)
import qualified Neovim.API.Text as Vim

unwrapArray :: (NvimObject o) => Vector Object -> [o]
unwrapArray = rights . Vector.toList . fmap fromObject

wrapArray :: (NvimObject o) => [o] -> Vector Object
wrapArray = Vector.fromList . fmap toObject

toPair :: (MonadIO io, NvimObject o1, NvimObject o2) => [Object] -> io (o1, o2)
toPair = fromObject' . ObjectArray

fromPair :: (NvimObject o1, NvimObject o2) => (o1, o2) -> Vector Object
fromPair (x, y) = Vector.fromList [toObject x, toObject y]

nvim_list_wins :: Neovim env [Window]
nvim_list_wins = unwrapArray <$> Vim.nvim_list_wins

window_get_cursor :: Window -> Neovim env (Int, Int)
window_get_cursor = (toPair . Vector.toList =<<) . Vim.window_get_cursor

window_set_cursor :: Window -> (Int, Int) -> Neovim env ()
window_set_cursor w = Vim.window_set_cursor w . fromPair

nvim_buf_get_lines :: Buffer -> Int64 -> Int64 -> Bool -> Neovim env [Text]
nvim_buf_get_lines b s e flag = unwrapArray <$> Vim.nvim_buf_get_lines b s e flag

nvim_buf_get_lines' :: Buffer -> Int64 -> Int64 -> Bool -> Neovim env (Vector Text)
nvim_buf_get_lines' b s e flag = Vector.fromList <$> nvim_buf_get_lines b s e flag

nvim_buf_set_text ::
    Buffer -> Int64 -> Int64 -> Int64 -> Int64 -> [Text] -> Neovim env ()
nvim_buf_set_text b sl sc el ec = Vim.nvim_buf_set_text b sl sc el ec . wrapArray
