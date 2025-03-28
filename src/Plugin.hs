{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Plugin where

import           Control.Lens
import           Control.Monad ((>=>))
import           Control.Monad.State.Class
import           Control.Monad.Trans
import           Cornelis.Agda (withCurrentBuffer, runIOTCM, withAgda, getAgda)
import           Cornelis.Diff (resetDiff, recordUpdate, Replace(..), Colline(..), Vallee(..))
import           Cornelis.Goals
import           Cornelis.Highlighting (getExtmarks, highlightInterval, updateLineIntervals)
import           Cornelis.InfoWin (showInfoWindow, closeInfoWindows)
import           Cornelis.Offsets
import           Cornelis.Pretty (prettyGoals, HighlightGroup (CornelisHole))
import           Cornelis.Types
import           Cornelis.Types.Agda hiding (Error)
import           Cornelis.Utils
import           Cornelis.Vim
import           Data.Bool (bool)
import           Data.Foldable (for_, fold, toList)
import           Data.IORef (IORef, readIORef, atomicModifyIORef)
import           Data.List
import qualified Data.Map as M
import           Data.Ord
import qualified Data.Text as T
import           Data.Traversable (for)
import qualified Data.Vector as V
import           Neovim
import           Neovim.API.Text
import           Neovim.User.Input (input)
import           System.Process (terminateProcess)
import           Text.Read (readMaybe)


runInteraction :: Interaction -> Neovim CornelisEnv ()
runInteraction interaction = withCurrentBuffer $ \b -> do
    agda <- getAgda b
    runIOTCM interaction agda


getDefinitionSites :: Buffer -> AgdaPos -> Neovim CornelisEnv (First DefinitionSite)
getDefinitionSites b p = withBufferStuff b $ \bs -> do
  marks <- getExtmarks b p
  pure $ flip foldMap marks $ \es ->
    First $ M.lookup (es_mark es) $ bs_goto_sites bs


doGotoDefinition :: CommandArguments -> Neovim CornelisEnv ()
doGotoDefinition _ = gotoDefinition


gotoDefinition :: Neovim CornelisEnv ()
gotoDefinition = withAgda $ do
  w <- nvim_get_current_win
  rc <- getWindowCursor w
  b <- window_get_buffer w
  getDefinitionSites b rc >>= \case
    First Nothing -> reportInfo "No syntax under cursor."
    First (Just ds) -> do
      -- TODO(sandy): escape spaces
      vim_command $ "edit " <> ds_filepath ds
      b' <- window_get_buffer w
      contents <- fmap (T.unlines . V.toList) $ buffer_get_lines b' 0 (-1) False
      let buffer_idx = toBytes contents $ zeroIndex $ ds_position ds
      -- TODO(sandy): use window_set_cursor instead?
      vim_command $ "keepjumps normal! " <> T.pack (show buffer_idx) <> "go"


doLoad :: CommandArguments -> Neovim CornelisEnv ()
doLoad = const load

atomicSwapIORef :: IORef a -> a -> IO a
atomicSwapIORef r x = atomicModifyIORef r (x,)

load :: Neovim CornelisEnv ()
load = withAgda $ withCurrentBuffer $ \b -> do
  agda <- getAgda b
  ready <- liftIO $ readIORef $ a_ready agda
  if ready then do
    vim_command "silent! noautocmd w"
    name <- buffer_get_name $ a_buffer agda
    flip runIOTCM agda $ Cmd_load name []
    buffer_get_number b >>= resetDiff
    updateLineIntervals b
  else vim_report_error "Agda is busy, not ready to load"

questionToMeta :: Buffer -> Neovim CornelisEnv ()
questionToMeta b = withBufferStuff b $ \bs -> do
  let ips = toList $ bs_ips bs

  res <- fmap fold $ for (sortOn (Down . iStart . ip_interval') ips) $ \ip -> do
    int <- getIpInterval b ip
    getGoalContentsMaybe b ip >>= \case
      -- We only don't have a goal contents if we are a ? goal
      Nothing -> do
        replaceInterval b int "{! !}"
        let int' = int { iEnd = iStart int `addCol` Offset 5 }
        void $ highlightInterval b int' CornelisHole
        modifyBufferStuff b $
          #bs_ips %~ M.insert (ip_id ip) (ip & #ip_intervalM . #_Identity .~ int')

        pure $ Any True
      Just _ -> pure $ Any False

  -- Force a save if we replaced any goals
  case getAny res of
    True -> load
    False -> pure ()


doAllGoals :: CommandArguments -> Neovim CornelisEnv ()
doAllGoals = const allGoals


allGoals :: Neovim CornelisEnv ()
allGoals =
  withAgda $ withCurrentBuffer $ \b ->
    withBufferStuff b $ \bs -> do
      goalWindow b $ bs_goals bs


doRestart :: CommandArguments -> Neovim CornelisEnv ()
doRestart _ = do
  bs <- gets cs_buffers
  modify $ #cs_buffers .~ mempty
  liftIO $ for_ bs $ terminateProcess . a_hdl . bs_agda_proc

doAbort :: CommandArguments -> Neovim CornelisEnv ()
doAbort _ = withAgda $ withCurrentBuffer $ getAgda >=> runIOTCM Cmd_abort

normalizationMode :: Neovim CornelisEnv Rewrite
normalizationMode = asks (cc_rewrite_mode . ce_config)

computeMode :: Neovim env ComputeMode
computeMode = pure DefaultCompute

solveOne :: CommandArguments -> Maybe String -> Neovim CornelisEnv ()
solveOne _ ms = withNormalizationMode ms $ \mode ->
  withAgda $ void $ withGoalAtCursor $ \b ip -> do
    agda <- getAgda b
    fp <- buffer_get_name b
    flip runIOTCM agda $
      Cmd_solveOne
        mode
        (ip_id ip)
        (mkAbsPathRnage fp $ ip_interval' ip)
        ""

autoOne :: CommandArguments -> Maybe String -> Neovim CornelisEnv ()
autoOne _ ms = withNormalizationMode ms $ \mode ->
  withAgda $ void $ withGoalAtCursor $ \b ip -> do
    agda <- getAgda b
    t <- getGoalContents b ip
    fp <- buffer_get_name b
    flip runIOTCM agda $
      Cmd_autoOne
        mode
        (ip_id ip)
        (mkAbsPathRnage fp $ ip_interval' ip)
        (T.unpack t)

withNormalizationMode :: Maybe String -> (Rewrite -> Neovim CornelisEnv ()) -> Neovim CornelisEnv ()
withNormalizationMode Nothing f = normalizationMode >>= f
withNormalizationMode (Just s) f =
  case readMaybe s of
    Nothing -> reportError $ "Invalid normalization mode: " <> T.pack s
    Just nm -> f nm

withComputeMode :: Maybe String -> (ComputeMode -> Neovim e ()) -> Neovim e ()
withComputeMode Nothing f = computeMode >>= f
withComputeMode (Just s) f =
  case readMaybe s of
    Nothing -> reportError $ "Invalid compute mode: "
      <> T.pack s
      <> ", expected one of "
      <> T.pack (show [(minBound :: ComputeMode) .. ])
    (Just cm) -> f cm

typeContext :: CommandArguments -> Maybe String -> Neovim CornelisEnv ()
typeContext _ ms = withNormalizationMode ms $ \mode ->
  withAgda $ void $ withGoalAtCursor $ \b goal -> do
    agda <- getAgda b
    fp <- buffer_get_name b
    flip runIOTCM agda $
      Cmd_goal_type_context
        mode
        (ip_id goal)
        (mkAbsPathRnage fp $ ip_interval' goal)
        ""

typeContextInfer :: CommandArguments -> Maybe String -> Neovim CornelisEnv ()
typeContextInfer _ ms = withNormalizationMode ms $ \mode ->
  withAgda $ void $ withGoalAtCursor $ \b ip -> do
    agda <- getAgda b
    fp <- buffer_get_name b
    contents <- getGoalContents b ip
    flip runIOTCM agda
      $ Cmd_goal_type_context_infer
          mode
          (ip_id ip)
          (mkAbsPathRnage fp $ ip_interval' ip)
      $ T.unpack contents

doRefine :: CommandArguments -> Neovim CornelisEnv ()
doRefine = const refine

refine :: Neovim CornelisEnv ()
refine = withAgda $ void $ withGoalAtCursor $ \b ip -> do
  agda <- getAgda b
  t <- getGoalContents b ip
  flip runIOTCM agda
    $ Cmd_refine_or_intro
        True
        (ip_id ip)
        -- We intentionally don't pass the range here; since doing so changes
        -- the response from Agda and requires a different codepath to perform
        -- the necessary edits.
        noRange
    $ T.unpack t

doGive :: CommandArguments -> Neovim CornelisEnv ()
doGive = const give

give :: Neovim CornelisEnv ()
give = withAgda $ void $ withGoalAtCursor $ \b ip -> do
  agda <- getAgda b
  t <- getGoalContents b ip
  flip runIOTCM agda
    $ Cmd_give
        WithoutForce
        (ip_id ip)
        -- We intentionally don't pass the range here; since doing so changes
        -- the response from Agda and requires a different codepath to perform
        -- the necessary edits.
        noRange
    $ T.unpack t

doElaborate :: CommandArguments -> Maybe String-> Neovim CornelisEnv ()
doElaborate _ ms = withNormalizationMode ms elaborate

elaborate :: Rewrite -> Neovim CornelisEnv ()
elaborate mode = withAgda $ void $ withGoalAtCursor $ \b ip -> do
  agda <- getAgda b
  fp <- buffer_get_name b
  t <- getGoalContents b ip
  flip runIOTCM agda
    $ Cmd_elaborate_give
        mode
        (ip_id ip)
        (mkAbsPathRnage fp $ ip_interval' ip)
    $ T.unpack t

doTypeInfer :: CommandArguments -> Maybe String -> Neovim CornelisEnv ()
doTypeInfer _ ms = withNormalizationMode ms inferType

inferType :: Rewrite -> Neovim CornelisEnv ()
inferType mode = withAgda $ do
    cmd <- withGoalContentsOrPrompt "Infer type of what?"
        (\goal -> pure . Cmd_infer mode (ip_id goal) NoRange)
        (pure . Cmd_infer_toplevel mode)
    runInteraction cmd


doWhyInScope :: CommandArguments -> Neovim CornelisEnv ()
doWhyInScope _ = do
  thing <- input "Why is what in scope? " Nothing Nothing
  whyInScope thing

whyInScope :: Text -> Neovim CornelisEnv ()
whyInScope thing = do
  withAgda $ void $ withCurrentBuffer $ \b -> do
    agda <- getAgda b
    flip runIOTCM agda $ Cmd_why_in_scope_toplevel $ T.unpack thing

doNormalize :: CommandArguments -> Maybe String -> Neovim CornelisEnv ()
doNormalize _ ms = withComputeMode ms $ \mode ->
  withAgda $ void $ do
    (b , goal) <- getGoalAtCursor
    agda <- getAgda b
    fp <- buffer_get_name b
    case goal of
        Nothing -> do
            thing <- input "Normalize what? " Nothing Nothing
            flip runIOTCM agda $ Cmd_compute_toplevel mode thing
        Just ip -> do
            t <- getGoalContents b ip
            flip runIOTCM agda
              $ Cmd_compute
                  mode
                  (ip_id ip)
                  (mkAbsPathRnage fp $ ip_interval' ip)
              $ T.unpack t

helperFunc :: Rewrite -> Text -> Neovim CornelisEnv ()
helperFunc mode expr = do
  withAgda $ void $ withGoalAtCursor $ \b ip -> do
    agda <- getAgda b
    fp <- buffer_get_name b
    flip runIOTCM agda
      $ Cmd_helper_function
          mode
          (ip_id ip)
          (mkAbsPathRnage fp $ ip_interval' ip)
      $ T.unpack expr

doHelperFunc :: CommandArguments -> Maybe String -> Neovim CornelisEnv ()
doHelperFunc _ ms = withNormalizationMode ms $ \mode -> do
  expr <- input "Expression: " Nothing Nothing
  helperFunc mode expr

doCaseSplit :: CommandArguments -> Neovim CornelisEnv ()
doCaseSplit _ = withAgda $ void $ withGoalAtCursor $ \b ip -> do
  contents <- fmap T.strip $ getGoalContents b ip
  thing <- bool (pure contents)
                (input @Text "Split on what?" Nothing Nothing)
         $ T.null contents
  caseSplit thing

caseSplit :: Text -> Neovim CornelisEnv ()
caseSplit thing = withAgda $ void $ withGoalAtCursor $ \b ip -> do
  agda <- getAgda b
  fp <- buffer_get_name b
  flip runIOTCM agda
    $ Cmd_make_case
        (ip_id ip)
        (mkAbsPathRnage fp $ ip_interval' ip)
    $ T.unpack thing

getVersion :: Neovim CornelisEnv ()
getVersion = withAgda $ void $ withCurrentBuffer $ getAgda >=> runIOTCM Cmd_show_version

doGetVersion :: CommandArguments -> Neovim CornelisEnv ()
doGetVersion = const getVersion

doQuestionToMeta :: CommandArguments -> Neovim CornelisEnv ()
doQuestionToMeta _ = withCurrentBuffer questionToMeta

goalWindow :: Buffer -> DisplayInfo ->  Neovim CornelisEnv ()
goalWindow b = showInfoWindow b . prettyGoals

computeModeCompletion :: String -> String -> Int -> Neovim env String
computeModeCompletion _ _ _ =
  pure $ unlines $ fmap show $ enumFromTo @ComputeMode minBound maxBound

rewriteModeCompletion :: String -> String -> Int -> Neovim env String
rewriteModeCompletion _ _ _ =
  pure $ unlines $ fmap show $ enumFromTo @Rewrite minBound maxBound

debugCommandCompletion :: String -> String -> Int -> Neovim env String
debugCommandCompletion _ _ _ =
  pure $ unlines $ fmap show $ enumFromTo @DebugCommand minBound maxBound


doDebug :: CommandArguments -> String -> Neovim CornelisEnv ()
doDebug _ str =
  case readMaybe str of
    Just DumpIPs ->
      withAgda $ withCurrentBuffer $ \b -> withBufferStuff b $ \bs -> do
        traceMX "ips" $ bs_ips bs
        traceMX "ipexts" $ bs_ip_exts bs
    Nothing ->
      vim_report_error $ T.pack $ "No matching debug command for " <> show str

-- | The @on_bytes@ callback required by @nvim_buf_attach@.
notifyEdit
  :: Text -- ^ the string "bytes"
  -> BufferNum -- ^ buffer handle
  -> Bool -- ^ b:changedtick
  -> Int -- ^ start row of the changed text (zero-indexed)
  -> Int -- ^ start column of the changed text
  -> Int -- ^ byte offset of the changed text (from the start of the buffer)
  -> Int -- ^ old end row of the changed text (relative to the start row)
  -> Int -- ^ old end column of the changed text
  -> Int -- ^ old end byte length of the changed text
  -> Int -- ^ new end row of the changed text (relative to the start row)
  -> Int -- ^ new end column of the changed text
  -> Int -- ^ new end byte length of the changed text
  -> Neovim CornelisEnv Bool  -- ^ Return True to detach
notifyEdit _ buf _ sr sc _ er ec _ fr fc _ = do
  recordUpdate buf (Replace (pos sr sc) (range er ec) (range fr fc))
  pure False
  where
    pos l c = Colline (toZeroIndexed l) (toZeroIndexed c)
    range l c = Vallee (Offset l) (Offset c)

doCloseInfoWindows :: CommandArguments -> Neovim CornelisEnv ()
doCloseInfoWindows = const closeInfoWindows
