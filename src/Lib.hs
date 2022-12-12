{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Lib where

import           Control.Arrow ((&&&))
import           Control.Concurrent.Chan.Unagi
import           Control.Lens
import           Control.Monad (forever)
import           Control.Monad (when)
import           Control.Monad.State.Class (gets, modify)
import           Cornelis.Config (getConfig)
import           Cornelis.Debug (reportExceptions)
import           Cornelis.Goals
import           Cornelis.Highlighting (highlightBuffer, getLineIntervals, lookupPoint)
import           Cornelis.InfoWin
import           Cornelis.Offsets
import           Cornelis.Subscripts (incNextDigitSeq, decNextDigitSeq)
import           Cornelis.Types
import           Cornelis.Utils
import           Cornelis.Vim
import           Data.Foldable (for_)
import           Data.IORef (newIORef)
import qualified Data.IntMap.Strict as IM
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Neovim
import           Neovim.API.Text
import           Neovim.Plugin (CommandOption(CmdComplete))
import           Plugin


getInteractionPoint :: Buffer -> Int -> Neovim CornelisEnv (Maybe (InteractionPoint Identity))
getInteractionPoint b i = gets $ preview $ #cs_buffers . ix b . #bs_ips . ix i


respondToHelperFunction :: DisplayInfo -> Neovim env ()
respondToHelperFunction (HelperFunction sig) = setreg "\"" sig
respondToHelperFunction _ = pure ()


addExtmarksToGoal :: Map AgdaInterval Extmark -> InteractionPoint Identity -> InteractionPoint Identity
addExtmarksToGoal m (InteractionPoint i f x) =
  InteractionPoint i f $ maybe x Just $ M.lookup (runIdentity f) m


respond :: Buffer -> Response -> Neovim CornelisEnv ()
-- Update the buffer's goal map
respond b (DisplayInfo dp) = do
  respondToHelperFunction dp
  when (dp & hasn't #_GoalSpecific) $
    modifyBufferStuff b $ #bs_goals .~ dp
  goalWindow b dp
-- Update the buffer's interaction points map
respond b (InteractionPoints ips) = do
  let ips' = IM.fromList $ fmap (ip_id &&& id)
           $ mapMaybe sequenceInteractionPoint ips
  oldips <- withBufferStuff b $ pure . bs_ips
  traceMX "old" oldips
  traceMX "new" ips'
  modifyBufferStuff b $ #bs_ips .~ ips'
-- Replace a function clause
respond b (MakeCase mkcase) = do
  doMakeCase b mkcase
  reload
-- Replace the interaction point with a result
respond b (GiveAction result ip) = do
  let i = ip_id ip
  getInteractionPoint b i >>= \case
    Nothing -> reportError $ T.pack $ "Can't find interaction point " <> show i
    Just ip' -> do
      int <- getIpInterval b ip'
      replaceInterval b int $ replaceQuestion result
  reload
-- Replace the interaction point with a result
respond b (SolveAll solutions) = do
  for_ solutions $ \(Solution i ex) ->
    getInteractionPoint b i >>= \case
      Nothing -> reportError $ T.pack $ "Can't find interaction point " <> show i
      Just ip -> do
        int <- getIpInterval b ip
        replaceInterval b int $ replaceQuestion ex
  reload
respond b ClearHighlighting = do
  -- delete what we know about goto positions
  modifyBufferStuff b $ #bs_goto_sites .~ mempty
  -- remove the extmarks and highlighting
  ns <- asks ce_namespace
  nvim_buf_clear_namespace b ns 0 (-1)
respond b (HighlightingInfo _remove hl) = do
  extmap <- highlightBuffer b hl
  when (not . null $ extmap) $ traceMX "extmap" extmap
  modifyBufferStuff b $
    #bs_ips %~ fmap (addExtmarksToGoal extmap)
respond _ (RunningInfo _ x) = reportInfo x
respond _ (ClearRunningInfo) = reportInfo ""
respond b (JumpToError _ pos) = do
  buf_lines <- nvim_buf_get_lines b 0 (-1) True
  let li = getLineIntervals buf_lines
  case lookupPoint li pos of
    Nothing -> reportError "invalid error report from Agda"
    Just (Pos l c) -> do
      ws <- fmap listToMaybe $ windowsForBuffer b
      for_ ws $ flip window_set_cursor (fromOneIndexed (oneIndex l), fromZeroIndexed c)
respond _ Status{} = pure ()
respond _ (Unknown k _) = reportError k

doMakeCase :: Buffer -> MakeCase -> Neovim CornelisEnv ()
doMakeCase b (RegularCase Function clauses ip) = do
  int' <- getIpInterval b ip
  let int = int' & #iStart . #p_col .~ toOneIndexed @Int 1
  ins <- getIndent b (zeroIndex (p_line (iStart int)))
  replaceInterval b int
    $ T.unlines
    $ fmap (T.replicate ins " " <>)
    $ fmap replaceQuestion clauses
-- TODO(sandy): It would be nice if Agda just gave us the bounds we're supposed to replace...
doMakeCase b (RegularCase ExtendedLambda clauses ip) = do
  ws <- windowsForBuffer b
  case listToMaybe ws of
    Nothing ->
      reportError
        "Unable to extend a lambda without having a window that contains the modified buffer. This is a limitation in cornelis."
    Just w -> do
      int' <- getIpInterval b ip
      Interval start end
        <- getLambdaClause w b (int' & #iStart . #p_col %~ (.+ Offset (- 1)))
           -- Subtract one so we are outside of a {! !} goal and the i} movement
           -- works correctly
      -- Add an extra character to the start so we leave a space after the
      -- opening brace, and subtract two characters from the end for the space and the }
      replaceInterval b (Interval (start & #p_col %~ (.+ Offset 1)) (end & #p_col %~ (.+ Offset (- 2))))
        $ T.unlines
        $ fmap replaceQuestion clauses & _tail %~ fmap (indent start)


------------------------------------------------------------------------------
-- | Indent a string with the given offset.
indent :: AgdaPos -> Text -> Text
indent (Pos _ c) s = T.replicate (fromZeroIndexed (zeroIndex c) - 1) " " <> "; " <> s


doPrevGoal :: CommandArguments -> Neovim CornelisEnv ()
doPrevGoal = const prevGoal

doNextGoal :: CommandArguments -> Neovim CornelisEnv ()
doNextGoal = const nextGoal

doToggleDebug :: CommandArguments -> Neovim CornelisEnv ()
doToggleDebug _ = modify $ #cs_debug %~ not

doIncNextDigitSeq :: CommandArguments -> Neovim CornelisEnv ()
doIncNextDigitSeq = const incNextDigitSeq

doDecNextDigitSeq :: CommandArguments -> Neovim CornelisEnv ()
doDecNextDigitSeq = const decNextDigitSeq


cornelisInit :: Neovim env CornelisEnv
cornelisInit = do
  (inchan, outchan) <- liftIO newChan
  ns <- nvim_create_namespace "cornelis"
  mvar <- liftIO $ newIORef $ CornelisState mempty False

  cfg <- getConfig

  let env = CornelisEnv mvar inchan ns cfg
  void $ withLocalEnv env $
    neovimAsync $ do
      forever $ reportExceptions $ do
        AgdaResp buffer next <- liftIO $ readChan outchan
        void $ neovimAsync $ reportExceptions $ respond buffer next
  pure env


-- Flush the TH environment
$(pure [])


main :: IO ()
main = neovim defaultConfig { plugins = [cornelis] }


cornelis :: Neovim () NeovimPlugin
cornelis = do
  env <- cornelisInit
  closeInfoWindows

  let rw_complete = CmdComplete "custom,InternalCornelisRewriteModeCompletion"
      cm_complete = CmdComplete "custom,InternalCornelisComputeModeCompletion"

  wrapPlugin $ Plugin
    { environment = env
    , exports =
        [ $(command "CornelisRestart"          'doRestart)        [CmdSync Async]
        , $(command "CornelisAbort"            'doAbort)          [CmdSync Async]
        , $(command "CornelisLoad"             'doLoad)           [CmdSync Async]
        , $(command "CornelisGoals"            'doAllGoals)       [CmdSync Async]
        , $(command "CornelisSolve"            'solveOne)         [CmdSync Async, rw_complete]
        , $(command "CornelisAuto"             'autoOne)          [CmdSync Async]
        , $(command "CornelisTypeContext"      'typeContext)      [CmdSync Async, rw_complete]
        , $(command "CornelisTypeContextInfer" 'typeContextInfer) [CmdSync Async, rw_complete]
        , $(command "CornelisMakeCase"         'doCaseSplit)      [CmdSync Async]
        , $(command "CornelisRefine"           'doRefine)         [CmdSync Async]
        , $(command "CornelisGive"             'doGive)           [CmdSync Async]
        , $(command "CornelisElaborate"        'doElaborate)      [CmdSync Async, rw_complete]
        , $(command "CornelisPrevGoal"         'doPrevGoal)       [CmdSync Async]
        , $(command "CornelisNextGoal"         'doNextGoal)       [CmdSync Async]
        , $(command "CornelisGoToDefinition"   'doGotoDefinition) [CmdSync Async]
        , $(command "CornelisWhyInScope"       'doWhyInScope)     [CmdSync Async]
        , $(command "CornelisNormalize"        'doNormalize)      [CmdSync Async, cm_complete]
        , $(command "CornelisHelperFunc"       'doHelperFunc)     [CmdSync Async, rw_complete]
        , $(command "CornelisQuestionToMeta"   'doQuestionToMeta) [CmdSync Async]
        , $(command "CornelisInc"              'doIncNextDigitSeq) [CmdSync Async]
        , $(command "CornelisDec"              'doDecNextDigitSeq) [CmdSync Async]
        , $(command "InternalCornelisToggleDebug"            'doToggleDebug) [CmdSync Async]
        , $(function "InternalCornelisRewriteModeCompletion" 'rewriteModeCompletion) Sync
        , $(function "InternalCornelisComputeModeCompletion" 'computeModeCompletion) Sync
        ]
    }

