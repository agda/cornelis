{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( module Utils
  , Edit (..)
  ) where

import           Control.Concurrent (threadDelay)
import           Cornelis.Types
import           Cornelis.Offsets
import           Cornelis.Utils (withLocalEnv)
import           Cornelis.Vim
import           Data.Foldable.Levenshtein (levenshtein, Edit(..))
import qualified Data.Text as T
import qualified Data.Vector as V
import           Lib
import           Neovim
import           Neovim.API.Text
import           Neovim.Test
import           Plugin
import           System.Exit (ExitCode(..))
import           System.FilePath (takeBaseName)
import           System.IO (hFlush, hPutStr)
import           System.IO.Temp (withSystemTempFile)
import           System.Process (rawSystem)
import           Test.Hspec hiding (after, before)


-- data Diff a
--   = Insert a
--   | Delete a
--   | Modify a a
--   deriving (Eq, Ord, Show, Functor)

isCopy :: Edit a -> Bool
isCopy Copy{} = True
isCopy _ = False

diff :: Show a => Eq a => [a] -> [a] -> [Edit a]
diff = ((filter (not . isCopy) . snd) .) . levenshtein @_ @_ @_ @Int
  -- (snd .) . go
  -- where
  --   go ([]) as = (length as, fmap Insert as)
  --   go bs [] = (length bs, fmap Delete bs)
  --   go (b : bs) (a : as)
  --     | b == a = go bs as
  --     | otherwise = do
  --         let x = traceShowId $ bimap (+1) (Delete b :) $ go bs (a : as)
  --             y = traceShowId $ bimap (+1) (Insert a :) $ go (b : bs) as
  --             z = traceShowId $ bimap (+1) (Modify b a :) $ go bs as
  --         minimumBy (comparing fst) [z, x, y]


differing :: Buffer -> Neovim env () -> Neovim env [Edit Text]
differing b m = do
  before <- fmap V.toList $ buffer_get_lines b 0 (-1) False
  m
  liftIO $ threadDelay 1e6
  after <- fmap V.toList $ buffer_get_lines b 0 (-1) False
  pure $ diff before after


intervention :: Buffer -> [Edit Text] -> Neovim env () -> Neovim env ()
intervention b d m = do
  d' <- differing b m
  liftIO $ d' `shouldBe` d

withNeovimEmbedded :: Seconds -> Neovim () a -> IO ()
withNeovimEmbedded secs = runInEmbeddedNeovim' def{cancelAfter = secs}

withVim :: Seconds -> (Window -> Buffer -> Neovim () ()) -> IO ()
withVim secs m = do
  withNeovimEmbedded secs $ do
    b <- nvim_create_buf False False
    w <- vim_get_current_window
    nvim_win_set_buf w b
    m w b


diffSpec
    :: String
    -> Seconds
    -> FilePath
    -> [Edit Text]
    -> (Window -> Buffer -> Neovim CornelisEnv ())
    -> Spec
diffSpec name secs fp diffs m =
  vimSpec name secs fp $ \w b -> intervention b diffs $ m w b


vimSpec
    :: String
    -> Seconds
    -> FilePath
    -> (Window -> Buffer -> Neovim CornelisEnv ())
    -> Spec
vimSpec name secs fp m = do
  it name $ do
    withSystemTempFile "test.agda" $ \fp' h -> do
      hPutStr h $ "module " <> takeBaseName fp' <> " where\n"
      hPutStr h . unlines . tail . lines =<< readFile fp
      hFlush h
      withNeovimEmbedded secs $ do
        env <- cornelisInit
        withLocalEnv env $ do
          nvim_command $ "noswap edit " <> T.pack fp'
          liftIO $ threadDelay 1e6
          w <- vim_get_current_window
          b <- nvim_win_get_buf w
          load
          liftIO $ threadDelay 5e6
          m w b

-- | Assert that 'bin' is executable.
executableSpec :: String -> Spec
executableSpec bin = describe bin $ do
  it "is executable" $ do
    exit_code <- rawSystem bin ["--version"]
    exit_code `shouldBe` ExitSuccess

goto :: Window -> Int -> Int -> Neovim env ()
goto w row col = setWindowCursor w $ Pos (toOneIndexed row) (toOneIndexed col)

