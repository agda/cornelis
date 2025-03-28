{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Cornelis.Types
  ( module Cornelis.Types
  , InteractionId
  , Buffer
  , Window
  , Text
  , traceMX
  , HasCallStack
  ) where

import Control.Concurrent.Chan.Unagi (InChan)
import Control.Monad.State.Class
import Cornelis.Debug
import Cornelis.Offsets (Pos(..), Interval(..), AgdaIndex, AgdaPos, AgdaInterval, VimIndex, LineNumber, Indexing(..))
import Cornelis.Types.Agda (InteractionId, Rewrite(..))
import Data.Aeson hiding (Error)
import Data.Char (toLower)
import Data.Functor.Identity
import Data.Generics.Labels ()
import Data.IORef
import Data.IntervalMap.FingerTree (IntervalMap)
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import DiffLoc (Diff, Colline)
import GHC.Generics
import GHC.Stack
import Neovim hiding (err)
import Neovim.API.Text (Buffer(..), Window)
import System.Process (ProcessHandle)

deriving stock instance Ord Buffer

data Agda = Agda
  { a_buffer :: Buffer
  , a_ready  :: IORef Bool
  , a_req    :: InChan String
  , a_hdl    :: ProcessHandle
  }
  deriving Generic

data BufferStuff = BufferStuff
  { bs_agda_proc  :: Agda
  , bs_ips        :: Map InteractionId (InteractionPoint Identity)
  , bs_ip_exts    :: Map InteractionId Extmark
  , bs_goto_sites :: Map Extmark DefinitionSite
  , bs_goals      :: DisplayInfo
  , bs_info_win   :: InfoBuffer
  , bs_code_map   :: LineIntervals
  }
  deriving Generic

newtype InfoBuffer = InfoBuffer
  { iw_buffer :: Buffer
  }
  deriving Generic

data CornelisState = CornelisState
  { cs_buffers :: Map Buffer BufferStuff
  , cs_diff :: Map BufferNum Diff0
  }
  deriving Generic

data SplitLocation
  = Vertical
  | Horizontal
  | OnLeft
  | OnRight
  | OnTop
  | OnBottom
  deriving (Eq, Ord, Enum, Bounded)

instance Show SplitLocation where
  show Vertical   = "vertical"
  show Horizontal = "horizontal"
  show OnLeft     = "left"
  show OnRight    = "right"
  show OnTop      = "top"
  show OnBottom   = "bottom"

readSplitLocation :: String -> Maybe SplitLocation
readSplitLocation s = case fmap toLower s of
  "vertical"   -> Just Vertical
  "horizontal" -> Just Horizontal
  "left"       -> Just OnLeft
  "right"      -> Just OnRight
  "top"        -> Just OnTop
  "bottom"     -> Just OnBottom
  _            -> Nothing

readRewrite :: String -> Maybe Rewrite
readRewrite s = case fmap toLower s of
  "asis"         -> Just AsIs
  "instantiated" -> Just Instantiated
  "headnormal"   -> Just HeadNormal
  "simplified"   -> Just Simplified
  "normalised"   -> Just Normalised
  _              -> Nothing

data CornelisConfig = CornelisConfig
  { cc_max_height :: Int64
  , cc_max_width :: Int64
  , cc_split_location :: SplitLocation
  , cc_rewrite_mode :: Rewrite
  }
  deriving (Show, Generic)

data CornelisEnv = CornelisEnv
  { ce_state :: IORef CornelisState
  , ce_stream :: InChan AgdaResp
  , ce_namespace :: Int64
  , ce_config :: CornelisConfig

  }
  deriving Generic

data AgdaResp = AgdaResp
  { ar_buffer :: Buffer
  , ar_message :: Response
  }
  deriving Generic

instance MonadState CornelisState (Neovim CornelisEnv) where
  get = do
    mv <- asks ce_state
    liftIO $ readIORef mv
  put a = do
    mv <- asks ce_state
    liftIO $ writeIORef mv a

data Response
  = DisplayInfo DisplayInfo
  | ClearHighlighting -- TokenBased
  | HighlightingInfo Bool [Highlight]
  | ClearRunningInfo
  | RunningInfo Int Text
  | Status
    { status_checked :: Bool
    , status_showIrrelevant :: Bool
    , status_showImplicits :: Bool
    }
  | JumpToError FilePath AgdaIndex
  | InteractionPoints [InteractionPoint Maybe]
  | GiveAction Text (InteractionPoint (Const ()))
  | MakeCase MakeCase
  | SolveAll [Solution]
  | Unknown Text Value
  deriving (Eq, Ord, Show)

data DefinitionSite = DefinitionSite
  { ds_filepath :: Text
  , ds_position :: AgdaIndex
  }
  deriving (Eq, Ord, Show)

instance FromJSON DefinitionSite where
  parseJSON = withObject "DefinitionSite" $ \obj ->
    DefinitionSite <$> obj .: "filepath" <*> obj .: "position"

data Highlight = Highlight
  { hl_atoms          :: [Text]
  , hl_definitionSite :: Maybe DefinitionSite
  , hl_start          :: AgdaIndex
  , hl_end            :: AgdaIndex
  }
  deriving (Eq, Ord, Show)

instance FromJSON Highlight where
  parseJSON = withObject "Highlight" $ \obj ->
    Highlight
      <$> obj .: "atoms"
      <*> obj .:? "definitionSite"
      <*> fmap (!! 0) (obj .: "range")
      <*> fmap (!! 1) (obj .: "range")

data MakeCase
  = RegularCase MakeCaseVariant [Text] (InteractionPoint Identity)
  deriving (Eq, Ord, Show, Generic)

data MakeCaseVariant = Function | ExtendedLambda
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON)

instance FromJSON MakeCase where
  parseJSON = withObject "MakeCase" $ \obj ->
    RegularCase <$> obj .: "variant" <*> obj .: "clauses" <*> obj .: "interactionPoint"


data Solution = Solution
  { s_ip :: InteractionId
  , s_expression :: Text
  }
  deriving (Eq, Ord, Show)

data InteractionPoint f = InteractionPoint
  { ip_id :: InteractionId
  , ip_intervalM :: f AgdaInterval
  } deriving Generic

deriving instance Eq (f AgdaInterval) => Eq (InteractionPoint f)
deriving instance Ord (f AgdaInterval) => Ord (InteractionPoint f)
deriving instance Show (f AgdaInterval) => Show (InteractionPoint f)

ip_interval' :: InteractionPoint Identity -> AgdaInterval
ip_interval' (InteractionPoint _ (Identity i)) = i

sequenceInteractionPoint :: Applicative f => InteractionPoint f -> f (InteractionPoint Identity)
sequenceInteractionPoint (InteractionPoint n f) = pure (InteractionPoint n) <*> fmap Identity f


data NamedPoint = NamedPoint
  { np_name :: Text
  , np_interval :: Maybe AgdaInterval
  }
  deriving (Eq, Ord, Show)

instance FromJSON AgdaInterval where
  parseJSON = withObject "IntervalWithoutFile" $ \obj -> do
    mkInterval <$> obj .: "start" <*> obj .: "end"
    where
      mkInterval (AgdaPos p) (AgdaPos q) = Interval p q

newtype AgdaPos' = AgdaPos AgdaPos

instance FromJSON AgdaPos' where
  parseJSON = withObject "Position" $ \obj -> do
    AgdaPos <$> (Pos <$> obj .: "line" <*> obj .: "col")

instance FromJSON (InteractionPoint Maybe) where
  parseJSON = withObject "InteractionPoint" $ \obj -> do
    InteractionPoint <$> obj .: "id" <*> fmap listToMaybe (obj .: "range")

instance FromJSON (InteractionPoint Identity) where
  parseJSON = withObject "InteractionPoint" $ \obj -> do
    InteractionPoint <$> obj .: "id" <*> fmap head (obj .: "range")

instance FromJSON (InteractionPoint (Const ())) where
  parseJSON = withObject "InteractionPoint" $ \obj -> do
    InteractionPoint <$> obj .: "id" <*> pure (Const ())

instance FromJSON NamedPoint where
  parseJSON = withObject "InteractionPoint" $ \obj -> do
    NamedPoint <$> obj .: "name" <*> fmap listToMaybe (obj .: "range")

instance FromJSON Solution where
  parseJSON = withObject "Solution" $ \obj ->
    Solution <$> obj .: "interactionPoint" <*> obj .: "expression"

data GoalInfo a = GoalInfo
  { gi_ip :: a
  , gi_type :: Type
  }
  deriving (Eq, Ord, Show, Functor)

instance FromJSON a => FromJSON (GoalInfo a) where
  parseJSON = withObject "GoalInfo" $ \obj ->
    (obj .: "kind") >>= \case
      "OfType" -> GoalInfo <$> obj .: "constraintObj" <*> obj .: "type"
      "JustSort" -> GoalInfo <$> obj .: "constraintObj" <*> pure (Type "Sort")
      (_ :: Text) -> empty

newtype Type = Type Text
  deriving newtype (Eq, Ord, Show, FromJSON)

data InScope = InScope
  { is_refied_name :: Text
  , is_original_name :: Text
  , is_in_scope :: Bool
  , is_type :: Type
  }
  deriving (Eq, Ord, Show)

instance FromJSON InScope where
  parseJSON = withObject "InScope" $ \obj ->
    InScope <$> obj .: "reifiedName" <*> obj .: "originalName" <*> obj .: "inScope" <*> obj .: "binding"

newtype Message = Message { getMessage :: Text }
  deriving (Eq, Ord, Show)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \obj ->
    Message <$> obj .: "message"


data DisplayInfo
  = AllGoalsWarnings
      { di_all_visible :: [GoalInfo (InteractionPoint Identity)]
      , di_all_invisible :: [GoalInfo NamedPoint]
      , di_errors :: [Message]
      , di_warnings :: [Message]
      }
  | GoalSpecific
      { di_ips :: InteractionPoint Identity
      , di_in_scope :: [InScope]
      , di_type :: Type
      , di_type_aux :: Maybe Type
      , di_boundary :: Maybe [Text]
      , di_output_forms :: Maybe [Text]
      }
  | HelperFunction Text
  | InferredType Type
  | DisplayError Text
  | IntroConstructorUnknown [Text]
  | WhyInScope Text
  | NormalForm Text
  | Version Text
  | UnknownDisplayInfo Value
  deriving (Eq, Ord, Show, Generic)

newtype TypeAux = TypeAux
  { ta_expr :: Type
  }

instance FromJSON TypeAux where
  parseJSON = withObject "TypeAux" $ \obj ->
    TypeAux . Type <$> obj .: "expr"

instance FromJSON DisplayInfo where
  parseJSON v = flip (withObject "DisplayInfo") v $ \obj ->
    obj .: "kind" >>= \case
      "AllGoalsWarnings" ->
        AllGoalsWarnings
          <$> obj .: "visibleGoals"
          <*> obj .: "invisibleGoals"
          <*> (obj .: "errors"   <|> fmap pure (obj .: "errors"))
          <*> (obj .: "warnings" <|> fmap pure (obj .: "warnings"))
      "Error" ->
        obj .: "error" >>= \err ->
          DisplayError <$> err .: "message"
      "IntroConstructorUnknown" -> IntroConstructorUnknown <$> obj .: "constructors"
      "InferredType" ->
        InferredType <$> obj .: "expr"
      "WhyInScope" ->
        WhyInScope <$> obj .: "message"
      "NormalForm" ->
        NormalForm <$> obj .: "expr"
      "GoalSpecific" ->
        (obj .: "goalInfo") >>= \info ->
          (info .: "kind") >>= \case
            "HelperFunction" ->
              HelperFunction <$> info .: "signature"
            "GoalType" ->
              GoalSpecific
                <$> obj .: "interactionPoint"
                <*> info .: "entries"
                <*> info .: "type"
                <*> (fmap (fmap ta_expr) (info .: "typeAux") <|> pure Nothing)
                <*> info .:? "boundary"
                <*> info .:? "outputForms"
            "NormalForm" -> NormalForm <$> info .: "expr"
            "InferredType" ->
              InferredType <$> info .: "expr"
            (_ :: Text) ->
              pure $ UnknownDisplayInfo v
      "Version" -> Version <$> obj .: "version"
      (_ :: Text) -> pure $ UnknownDisplayInfo v

instance FromJSON Response where
  parseJSON v = flip (withObject "Response") v $ \obj -> do
    obj .: "kind" >>= \case
      "MakeCase" ->
        MakeCase <$> parseJSON v
      "ClearRunningInfo" ->
        pure ClearRunningInfo
      "HighlightingInfo" ->
        (obj .: "info") >>= \info ->
          HighlightingInfo <$> info .: "remove" <*> info .: "payload"
      "ClearHighlighting" ->
        pure ClearHighlighting
      "RunningInfo" ->
        RunningInfo <$> obj .: "debugLevel" <*> obj .: "message"
      "InteractionPoints" ->
        InteractionPoints <$> obj .: "interactionPoints"
      "SolveAll" ->
        SolveAll <$> obj .: "solutions"
      "GiveAction" ->
        (obj .: "giveResult") >>= \result ->
          GiveAction <$> result .: "str" <*> obj .: "interactionPoint"
      "DisplayInfo" ->
        DisplayInfo <$> obj .: "info"
      "JumpToError" ->
        JumpToError <$> obj .: "filepath" <*> obj .: "position"
      "Status" -> do
        (obj .: "status" >>=) $ withObject "Status" $ \s ->
          Status <$> s .: "checked"
                  <*> (s .: "showIrrelevantArguments"<|> pure False)
                  <*> s .: "showImplicitArguments"
      (_ :: Text) -> Unknown <$> obj .: "kind" <*> pure v

newtype Extmark = Extmark Int64
  deriving (Eq, Ord, Show)

data ExtmarkStuff = ExtmarkStuff
  { es_mark     :: Extmark
  , es_hlgroup  :: Text
  , es_interval :: AgdaInterval
  }
  deriving (Eq, Ord, Show)


data DebugCommand
  = DumpIPs
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- * Translating Agda indices to Vim indices
--
-- Agda sometimes takes a while to process. We want to be able to modify the
-- buffer in the meantime. But then, Agda's highlighting instructions
-- will be out of sync.
--
-- We record the LineIntervals mapping (in field 'bs_code_map') at the time
-- when we reloaded, to translate old AgdaIndex to old VimPos, and then use the
-- 'Diff0' data structure (in field 'cs_diff') to translate (intervals of) old
-- VimPos to new VimPos.

-- | Data for mapping code point indices to byte indices
newtype LineIntervals = LineIntervals
  { li_intervalMap :: IntervalMap AgdaIndex (LineNumber 'ZeroIndexed, Text)
    -- ^ Mapping from positions to line numbers
  } deriving newtype (Semigroup, Monoid)

-- | Buffer update events give us this instead of a proper Buffer
-- There is  buffer_get_number :: Buffer -> Neovim env BufferNum
-- but nothing the other way???
type BufferNum = Int64

-- Data structures from the diff-loc library
type DPos = Colline (LineNumber 'ZeroIndexed) VimIndex
type Diff0 = Diff DPos
