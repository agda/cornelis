{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Cornelis.Highlighting where

import           Control.Lens ((<>~), (.~))
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Maybe
import           Cornelis.Diff
import           Cornelis.Offsets
import           Cornelis.Pretty
import           Cornelis.Types
import           Cornelis.Utils
import           Cornelis.Vim (unvimify, vimify)
import qualified Cornelis.Vim.Compat as Compat
import           Data.Coerce (coerce)
import           Data.IntervalMap.FingerTree (IntervalMap)
import qualified Data.IntervalMap.FingerTree as IM
import qualified Data.Map as M
import           Data.Maybe (listToMaybe, catMaybes, mapMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Traversable (for)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Neovim
import           Neovim.API.Text

lineIntervalsForBuffer :: Buffer -> Neovim CornelisEnv LineIntervals
lineIntervalsForBuffer b = do
  buf_lines <- Compat.nvim_buf_get_lines b 0 (-1) True
  pure $ getLineIntervals (V.fromList buf_lines)

updateLineIntervals :: Buffer -> Neovim CornelisEnv ()
updateLineIntervals b = do
  li <- lineIntervalsForBuffer b
  modifyBufferStuff b $ #bs_code_map .~ li

highlightBuffer :: Buffer -> [Highlight] -> Neovim CornelisEnv (M.Map AgdaInterval Extmark)
highlightBuffer b hs = withBufferStuff b $ \bs -> do
  let li = bs_code_map bs
  (holes, exts) <- fmap unzip $ for hs $ \hl -> do
    (hole, mext) <- addHighlight b li hl
    pure (hole, (hl, mext))

  let zs = catMaybes $ do
        (hl, mext) <- exts
        pure $ do
          ext <- mext
          ds <- hl_definitionSite hl
          pure (ext, ds)

  modifyBufferStuff b $ #bs_goto_sites <>~ M.fromList zs
  pure $ mconcat holes

getLineIntervals :: Vector Text -> LineIntervals
getLineIntervals = LineIntervals . go (toOneIndexed @Int 1) (toZeroIndexed @Int 0)
  where
    go
        :: AgdaIndex
        -> LineNumber 'ZeroIndexed
        -> Vector Text
        -> IntervalMap AgdaIndex (LineNumber 'ZeroIndexed, Text)
    go pos line v
      | Just (t, ss) <- V.uncons v =
        let len = T.length t
            pos' = pos .+ Offset len
        in IM.insert (IM.Interval pos pos') (line, t)
              $ go (incIndex pos') (incIndex line) ss
      | otherwise = mempty

lookupPoint :: LineIntervals -> AgdaIndex -> Maybe VimPos
lookupPoint (LineIntervals im) i = do
  (IM.Interval lineStart _, (line, s)) <- listToMaybe $ IM.search i im
  let col = toBytes s (toZeroIndexed @Int 0 .+ (i .-. lineStart))
  pure (Pos line col)

------------------------------------------------------------------------------
-- | Returns any holes it tried to highlight on the left
addHighlight
    :: Buffer
    -> LineIntervals
    -> Highlight
    -> Neovim CornelisEnv (M.Map AgdaInterval Extmark, Maybe Extmark)
addHighlight b lis hl = do
  case Interval
           <$> lookupPoint lis (hl_start hl)
           <*> lookupPoint lis (hl_end hl) of
    Just int@(Interval start end) -> do
      ext <- setHighlight b int $ parseHighlightGroup hl

      fmap (, ext) $ case isHole hl of
        False -> pure mempty
        True -> do
          let vint = Interval start end
          aint <- traverse (unvimify b) vint
          pure $ maybe mempty (M.singleton aint) ext
    Nothing -> pure (mempty, Nothing)
  where
    -- Convert the first atom in a reply to a custom highlight
    -- group, and return whether it is a hole or not.
    --
    -- Note that Agda returns both "aspects" for regular syntax
    -- and "other aspects" for error messages, warnings and hints.
    -- Currently, the latter kind takes precedence, since it is
    -- located first in the message returned by Agda.
    --
    -- See 'Cornelis.Pretty.HighlightGroup' for more details.
    --
    -- TODO: Investigate whether is is possible/feasible to
    -- attach multiple HL groups to buffer locations.
    parseHighlightGroup :: Highlight -> Maybe HighlightGroup
    parseHighlightGroup = listToMaybe . mapMaybe atomToHlGroup . hl_atoms

    isHole :: Highlight -> Bool
    isHole = elem "hole" . hl_atoms

setHighlight
    :: Buffer
    -> Interval VimPos
    -> Maybe HighlightGroup
    -> Neovim CornelisEnv (Maybe Extmark)
setHighlight b i hl = do
  bn <- buffer_get_number b
  mi' <- translateInterval bn i
  case mi' of
    Just i' -> setHighlight' b i' hl
    Nothing -> pure Nothing

setHighlight'
    :: Buffer
    -> Interval VimPos
    -> Maybe HighlightGroup
    -> Neovim CornelisEnv (Maybe Extmark)
setHighlight' b (Interval (Pos sl sc) (Pos el ec)) hl = do
  ns <- asks ce_namespace
  let from0 = fromZeroIndexed
  flip catchNeovimException (const (pure Nothing))
    $ fmap (Just . coerce)
    $ nvim_buf_set_extmark b ns (from0 sl) (from0 sc)
    $ M.fromList
    $ [ ( "end_line"
        , ObjectInt $ from0 el
        )
      , ( "end_col"
        , ObjectInt $ from0 ec
        )
      ] ++
      concatMap (\hl' ->
          [ ( "hl_group"
            , ObjectString $ encodeUtf8 $ T.pack $ show hl'
            )
          , ( "priority"
            , ObjectInt $ priority hl'
            )
          ])
          hl


highlightInterval
    :: Buffer
    -> AgdaInterval
    -> HighlightGroup
    -> Neovim CornelisEnv (Maybe Extmark)
highlightInterval b int hl = do
  int' <- traverse (vimify b) int
  setHighlight b int' $ Just hl


parseExtmark :: Buffer -> Object -> Neovim CornelisEnv (Maybe ExtmarkStuff)
parseExtmark b
  (ObjectArray ( (objectToInt -> Just ext)
               : (objectToInt @Int -> Just (toZeroIndexed -> start_line))
               : (objectToInt @Int -> Just (toZeroIndexed -> start_col))
               : ObjectMap details
               : _
               )) = runMaybeT $ do
  end_col <- hoistMaybe $ fmap toZeroIndexed
            . objectToInt @Int =<< M.lookup (ObjectString "end_col") details
  end_line <- hoistMaybe $ fmap toZeroIndexed
            . objectToInt @Int =<< M.lookup (ObjectString "end_row") details
  hlgroup <- hoistMaybe $ objectToText =<< M.lookup (ObjectString "hl_group") details
  int <- lift $ traverse (unvimify b) (Interval (Pos start_line start_col) (Pos end_line end_col))
  pure $ ExtmarkStuff
    { es_mark = Extmark ext
    , es_hlgroup = hlgroup
    , es_interval = int
    }
parseExtmark _ _ = pure Nothing

#if __GLASGOW_HASKELL__ <= 904
hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
#endif


getExtmarks :: Buffer -> AgdaPos -> Neovim CornelisEnv [ExtmarkStuff]
getExtmarks b p = do
  ns <- asks ce_namespace
  vp <- vimify b p
  let -- i = 0 for beginning of line, i = -1 for end of line
      pos i = ObjectArray [ ObjectInt $ fromZeroIndexed (p_line vp)
                          , ObjectInt i
                          ]
  res <- nvim_buf_get_extmarks b ns (pos 0) (pos (-1)) $ M.singleton "details" $ ObjectBool True
  marks <- fmap catMaybes $ traverse (parseExtmark b) $ V.toList res

  pure $ marks >>= \es ->
    case containsPoint (es_interval es) p of
      False -> mempty
      True -> [es]

