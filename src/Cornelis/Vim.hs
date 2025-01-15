{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Cornelis.Vim where

import           Control.Lens ((%~), _head, _last, (&))
import           Cornelis.Offsets
import           Cornelis.Types
import           Cornelis.Utils (objectToInt, savingCurrentPosition, savingCurrentWindow)
import           Data.Either.Combinators (rightToMaybe)
import           Data.Foldable (toList)
import           Data.Int
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import           Neovim
import           Neovim.API.Text


vimFirstLine :: Int64
vimFirstLine = 0

vimLastLine :: Int64
vimLastLine = -1

getWindowCursor :: Window -> Neovim env AgdaPos
getWindowCursor w = do
  (toOneIndexed -> row, toZeroIndexed -> col) <- window_get_cursor w
  -- window_get_cursor gives us a 1-indexed line, but that is the same way that
  -- lines are indexed.
  let line = zeroIndex row
  b <- window_get_buffer w
  unvimify b (Pos line col)

-- | TODO(sandy): POSSIBLE BUG HERE. MAKE SURE YOU SET THE CURRENT WINDOW
-- BEFORE CALLING THIS FUNCTION
getpos :: Buffer -> Char -> Neovim env AgdaPos
getpos b mark = do
  -- getpos gives us a (1,1)-indexed position!
  ObjectArray [_, objectToInt @Int -> Just (toOneIndexed -> line), objectToInt @Int -> Just (toOneIndexed -> col), _]
    <- vim_call_function "getpos" $ V.fromList [ObjectString $ encodeUtf8 $ T.singleton mark]
  unvimify b (Pos (zeroIndex line) (zeroIndex col))

data SearchMode = Forward | Backward
  deriving (Eq, Ord, Show)

searchpos :: Buffer -> [Text] -> SearchMode -> Neovim env AgdaPos
searchpos b pats dir = do
  -- unlike getpos, these columns are 0 indexed W T F
  ObjectArray [objectToInt @Int -> Just (toOneIndexed -> line), objectToInt @Int -> Just (toZeroIndexed -> col)]
    <- vim_call_function "searchpos" $ V.fromList
        [ ObjectString $ encodeUtf8 $ T.intercalate "\\|" pats
        , ObjectString $ encodeUtf8 $ case dir of
            Forward -> "n"
            Backward -> "bn"
        ]
  unvimify b (Pos (zeroIndex line) col)

setWindowCursor :: Window -> AgdaPos -> Neovim env ()
setWindowCursor w p = do
  b <- window_get_buffer w
  Pos l c <- vimify b p
  window_set_cursor w (fromOneIndexed (oneIndex l), fromZeroIndexed c)

replaceInterval :: Buffer -> Interval AgdaPos -> Text -> Neovim env ()
replaceInterval b ival str
  = do
    Interval (Pos sl sc) (Pos el ec) <- traverse (vimify b) ival
    nvim_buf_set_text b (from0 sl) (from0 sc) (from0 el) (from0 ec) $ V.fromList $ T.lines str
  where
    from0 = fromZeroIndexed

------------------------------------------------------------------------------
-- | Vim insists on returning byte-based offsets for the cursor positions...
-- why the fuck? This function undoes the problem.
unvimify :: Buffer -> VimPos -> Neovim env AgdaPos
unvimify b (Pos line col) = do
  txt <- getBufferLine b line
  let col' = fromBytes txt col
  pure (Pos (oneIndex line) (oneIndex col'))

vimify :: Buffer -> AgdaPos -> Neovim env VimPos
vimify b (Pos (zeroIndex -> line) (zeroIndex -> col)) = do
  txt <- getBufferLine b line
  let col' = toBytes txt col
  pure (Pos line col')

getIndent :: Buffer -> LineNumber 'ZeroIndexed -> Neovim env Int
getIndent b l = do
  txt <- getBufferLine b l
  pure $ T.length $ T.takeWhile (== ' ') txt


getBufferLine :: Buffer -> LineNumber 'ZeroIndexed -> Neovim env Text
getBufferLine b l = buffer_get_line b (fromZeroIndexed l)

getBufferInterval :: Buffer -> Interval AgdaPos -> Neovim env Text
getBufferInterval b (Interval start end) = do
    Pos sl _ <- vimify b start
    Pos el _ <- vimify b end
    -- nvim_buf_get_lines is exclusive in its end line, thus the plus 1
    ls <- fmap toList $ nvim_buf_get_lines b (from0 sl) (from0 el + 1) False
    pure $ T.unlines $
      ls & _last %~ T.take (from1 (p_col end))
         & _head %~ T.drop (from1 (p_col start))
  where
    from0 = fromZeroIndexed
    from1 = fromZeroIndexed . zeroIndex  -- add 1 to a one-indexed arg before passing it to take/drop

reportError :: Text -> Neovim env ()
reportError = vim_report_error

reportInfo :: Text -> Neovim env ()
reportInfo m = vim_out_write $ m <> "\n"

setreg :: Text -> Text -> Neovim env ()
setreg reg val
  = void
  $ vim_call_function "setreg"
  $ V.fromList
    [ ObjectString $ encodeUtf8 reg
    , ObjectString $ encodeUtf8 val
    ]

getExtmarkIntervalById :: Int64 -> Buffer -> Extmark -> Neovim env (Maybe AgdaInterval)
getExtmarkIntervalById ns b (Extmark x) = do
  res
    <- nvim_call_function "nvim_buf_get_extmark_by_id"
     $ V.fromList
     $ b +: ns +: x +: M.singleton @Text "details" True +: []
  case res of
    ObjectArray [ objectToInt @Int -> Just (toZeroIndexed -> sline)
                , objectToInt @Int -> Just (toZeroIndexed -> scol)
                , ObjectMap details
                ] -> do
      let toZ = fmap toZeroIndexed . objectToInt @Int
          Just eline = toZ $ details M.! ObjectString "end_row"
          Just ecol  = toZ $ details M.! ObjectString "end_col"
      fmap Just $ traverse (unvimify b) $ Interval (Pos sline scol) $ Pos eline ecol
    _ -> pure Nothing

getreg :: Text -> Neovim env (Maybe Text)
getreg reg
    = (rightToMaybe . fromObject)
    <$> (vim_call_function "getreg" $ V.singleton $ toObject reg)

------------------------------------------------------------------------------
-- | Awful function that does the motion in visual mode and gives you back
-- where vim thinks the @'<@ and @'>@ marks are.
--
-- I'm so sorry.
getSurroundingMotion
    :: Window
    -> Buffer
    -> Text
    -> AgdaPos
    -> Neovim env (AgdaPos, AgdaPos)
getSurroundingMotion w b motion p = do
  savingCurrentWindow $ do
    savingCurrentPosition w $ do
      nvim_set_current_win w
      setWindowCursor w p
      vim_command $ "normal v" <> motion
      start <- getpos b 'v'
      end <- getpos b '.'
      void $ nvim_input "<esc>"
      pure (start, end)

------------------------------------------------------------------------------
-- | Get an interval to replace for a lambda case split
getLambdaClause
    :: Window
    -> Buffer
    -> AgdaInterval
    -> Neovim env AgdaInterval
getLambdaClause w b (Interval p0 p1) = do
  savingCurrentWindow $ do
    savingCurrentPosition w $ do
      nvim_set_current_win w
      setWindowCursor w p0
      start <- searchpos b ["{", ";"] Backward
      setWindowCursor w p1
      end <- searchpos b [";", "}"] Forward
      pure (Interval start end)

