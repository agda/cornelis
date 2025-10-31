{-# LANGUAGE OverloadedStrings #-}

module Cornelis.Pretty where

import           Cornelis.Offsets (AgdaPos, Pos(..), Interval(..), AgdaInterval, charToBytes, textToBytes)
import qualified Cornelis.Types as C
import qualified Cornelis.Types as X
import           Cornelis.Types hiding (Type)
import           Data.Bool (bool)
import           Data.Function (on)
import           Data.Int
import           Data.List (sortOn, groupBy, intersperse)
import qualified Data.Map as M
import           Data.Maybe (maybeToList, fromMaybe, fromJust)
import           Data.Semigroup (stimes)
import qualified Data.Text as T
import           Prettyprinter
import           Prettyprinter.Internal.Type


-- | Custom highlight groups set by the plugin.
--
-- These groups linked to Neovim default highlight groups in @syntax/agda.vim@.
--
-- The suffix after @Cornelis/XXX/@ matches the names as returned by Agda, contained in 'C.hl_atoms'.
-- How these names are generated is not documented on Agda's side, but the implementation can be found at
-- ['Agda.Interaction.Highlighting.Common.toAtoms'](https://github.com/agda/agda/blob/2ee8d18194b6f6db1f144c12cb177dbc17089d5e/src/full/Agda/Interaction/Highlighting/Common.hs#L19).
-- The corresponding Agda types are found in 'Agda.Syntax.Common.Aspect'.
--
-- NOTE:
--  * When modifying this type, remember to sync the changes to @syntax/agda.vim@
--  * This list of highlight groups does not yet cover all variants returned by Agda
data HighlightGroup
  = CornelisError -- ^ InfoWin highlight group of error messages
  | CornelisErrorWarning -- ^ InfoWin highlight group of warnings considered fatal
  | CornelisWarn -- ^ InfoWin highlight group of warnings
  | CornelisTitle -- ^ InfoWin highlight of section titles
  | CornelisName -- ^ InfoWin highlight of names in context
  | CornelisHole -- ^ An open hole (@{! /.../ !}@ and @?@)
  | CornelisUnsolvedMeta -- ^ An unresolved meta variable
  | CornelisUnsolvedConstraint -- ^ An unresolved constraint
  | CornelisTerminationProblem -- ^ Failed termination check
  | CornelisPositivityProblem -- ^ Failed positivity check
  | CornelisKeyword -- ^ An Agda keywords (@where@, @let@, etc.)
  | CornelisSymbol -- ^ A symbol, not part of an identifier (@=@, @:@, @{@, etc.)
  | CornelisType -- ^ A datatype (@Nat@, @Bool@, etc.)
  | CornelisPrimitiveType -- ^ A primitive/builtin Agda type
  | CornelisRecord -- ^ A datatype, defined as a @record@
  | CornelisFunction -- ^ A function, e.g. a top-level declaration
  | CornelisArgument -- ^ The name of an (implicit) argument, i.e. @Foo {/bar/ = 42}@
  | CornelisBound -- ^ A bound identifier, e.g. a variable in a pattern clause or a module parameter
  | CornelisOperator -- ^ A mixfix operator, e.g. @x /∷/ xs@.
  | CornelisField -- ^ Field of a record definition
  | CornelisGeneralizable -- ^ A generalizable variable, defined for example in a @variable@ block
  | CornelisMacro -- ^ A macro, defined in a @macro@ block
  | CornelisInductiveConstructor -- ^ A constructor of an inductive data type (e.g. @data Foo /.../@)
  | CornelisCoinductiveConstructor -- ^ A constructor for a /co/inductive datatype, i.e. a record marked @coinductive@
  | CornelisNumber -- ^ A number literal
  | CornelisComment -- ^ A comment
  | CornelisString -- ^ A string literal
  | CornelisCatchAllClause -- ^ "Catch-all clause". Some sort of fallback; not exactly clear where this is used
  | CornelisTypeChecks -- ^ A location being type-checked right now
  | CornelisModule -- ^ A module name, e.g. in a @module@ block or an @import@
  | CornelisPostulate -- ^ A term, defined in a @postulate@ block
  | CornelisPrimitive -- ^ An Agda primitive, e.g. @Set@
  | CornelisPragma -- ^ The argument to a pragma, e.g. @{-# OPTIONS /--foo/ -#}@
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Priority of the HighlightGroup. See `:h vim.highlight.priorities` for more information.
--
-- 100 is the default syntax highlighting priority, while 150 is reserved for diagnostics.
priority :: HighlightGroup -> Int64
priority CornelisError              = 150
priority CornelisErrorWarning       = 150
priority CornelisWarn               = 150
priority CornelisUnsolvedMeta       = 150
priority CornelisUnsolvedConstraint = 150
priority CornelisTerminationProblem = 150
priority CornelisPositivityProblem  = 150
priority _                          = 100

atomToHlGroup :: Text -> Maybe HighlightGroup
atomToHlGroup atom = M.lookup atom allHlGroups where
    stripCornelis = fromJust . T.stripPrefix "Cornelis"

    toAtomName :: HighlightGroup -> Text
    toAtomName hlGroup = T.toLower $ stripCornelis $ T.pack $ show hlGroup

    allHlGroups :: M.Map Text HighlightGroup
    allHlGroups = M.fromList $
        map (\g -> (toAtomName g, g)) [(minBound :: HighlightGroup) ..]

data InfoHighlight a = InfoHighlight
  { ihl_start :: (Int64, Int64)
  , ihl_end :: a
  , ihl_group :: HighlightGroup
  }
  deriving (Eq, Ord, Show, Functor)

spanInfoHighlights
    :: InfoHighlight (Int64, Int64)
    -> [InfoHighlight Int64]
spanInfoHighlights ih@(InfoHighlight (sl, sc) (el, ec) hg)
  | sl == el = pure $ fmap snd ih
  | otherwise
      = InfoHighlight (sl, sc) (-1) hg
      : InfoHighlight (el, 0) ec hg
      : fmap (\l -> InfoHighlight (l, 0) (-1) hg) [sl + 1 .. el - 1]


renderWithHlGroups
    :: SimpleDocStream HighlightGroup
    -> ([InfoHighlight (Int64, Int64)], SimpleDocStream a)
renderWithHlGroups = go [] 0 0
  where
    go
      :: [InfoHighlight ()]
      -> Int64
      -> Int64
      -> SimpleDocStream HighlightGroup
      -> ([InfoHighlight (Int64, Int64)], SimpleDocStream a)
    go _ _ _ SFail = pure SFail
    go _ _ _ SEmpty = pure SEmpty
    go st r c (SChar c' sds) =
      SChar c' <$> go st r (c + fromIntegral (charToBytes c')) sds
    go st r c (SText n txt sds) =
      SText n txt <$> go st r (c + fromIntegral (textToBytes txt)) sds
    go st r _ (SLine n sds) = SLine n <$> go st (r + 1) (fromIntegral n) sds
    go st r c (SAnnPush hg sds) = go (InfoHighlight (r, c) () hg : st) r c sds
    go [] _ _ (SAnnPop _) = error "popping an annotation that doesn't exist"
    go (ih : ihs) r c (SAnnPop sds) = do
      sds' <- go ihs r c sds
      ([(r, c) <$ ih], sds')


prettyType :: C.Type -> Doc HighlightGroup
prettyType (C.Type ty) = annotate CornelisType $ sep $ fmap pretty $ T.lines ty


groupScopeSet :: [InScope] -> [[InScope]]
groupScopeSet
  = sortOn (is_refied_name . head)
  . fmap (sortOn is_refied_name)
  . groupBy (on (==) is_type)
  . sortOn is_type

prettyGoals :: DisplayInfo -> Doc HighlightGroup
prettyGoals (AllGoalsWarnings vis invis errs warns) =
  vcat $ punctuate hardline $ filter (not . isEmpty)
    [ section "Warnings" warns $ annotate CornelisWarn . pretty . getMessage
    , section "Visible Goals" vis $
        prettyGoal . fmap (mappend "?" . T.pack . show . ip_id)
    , section "Errors" errs prettyError
    , section "Invisible Goals" invis $ \gi ->
        prettyGoal (fmap np_name gi)
          <+> maybe mempty (brackets . ("at" <+>) . prettyInterval) (np_interval $ gi_ip gi)
    ]
prettyGoals (GoalSpecific _ scoped ty mhave mboundary mconstraints) =
  vcat $ intersperse (stimes @_ @Int 60 "—") $
    [ section "Boundary" (fromMaybe [] mboundary) pretty
    ] <>
    [ annotate CornelisTitle "Goal:" <+> prettyType ty
    ] <>
    [ annotate CornelisTitle "Have:" <+> prettyType have
    | have <- maybeToList mhave
    ] <>
    [ vcat $ fmap prettyInScopeSet $ groupScopeSet scoped
    ] <>
    [ section "Constraints" (fromMaybe [] mconstraints) pretty
    ]
prettyGoals (HelperFunction sig) =
  section "Helper Function"
    [ mempty
    , annotate CornelisType $ pretty sig
    , mempty
    , annotate CornelisComment $ parens "copied to \" register"
    ] id
prettyGoals (InferredType ty) =
  annotate CornelisTitle "Inferred Type:" <+> prettyType ty
prettyGoals (WhyInScope msg) = pretty msg
prettyGoals (NormalForm expr) = pretty expr
prettyGoals (DisplayError err) = annotate CornelisError $ pretty err
prettyGoals (IntroConstructorUnknown constructors) = vsep
  [ annotate CornelisErrorWarning "Don't know which constructor to introduce."
  , mempty
  , section "Constructors available" constructors prettyName
  ]
prettyGoals (UnknownDisplayInfo v) = annotate CornelisError $ pretty $ show v

prettyInterval :: AgdaInterval -> Doc HighlightGroup
prettyInterval (Interval s e)
  | p_line s == p_line e
  = prettyPoint s <> "-" <> pretty (p_col e)
  | otherwise
  = prettyPoint s <> "-" <> prettyPoint e

prettyPoint :: AgdaPos -> Doc HighlightGroup
prettyPoint p = pretty (p_line p) <> "," <> pretty (p_col p)


isEmpty :: Doc HighlightGroup -> Bool
isEmpty Empty = True
isEmpty _ = False


section
    :: Doc HighlightGroup
    -> [a]
    -> (a -> Doc HighlightGroup)
    -> Doc HighlightGroup
section _ [] _ = mempty
section doc as f = vcat $
  annotate CornelisTitle (doc <> ":") : fmap f as


prettyName :: Text -> Doc HighlightGroup
prettyName = prettyVisibleName True


prettyVisibleName :: Bool -> Text -> Doc HighlightGroup
prettyVisibleName False t = annotate CornelisComment $ "(" <> pretty t <> ")"
prettyVisibleName True t = annotate CornelisName $ pretty t

prettyInScope :: InScope -> Doc HighlightGroup
prettyInScope (InScope reified _ in_scope ty) =
  hsep
    [ prettyGoal $ GoalInfo reified ty
    , bool
        (pretty (replicate 6 ' ') <+> annotate CornelisComment (parens "not in scope"))
        mempty
        in_scope
    ]

prettyInScopeSet :: [InScope] -> Doc HighlightGroup
prettyInScopeSet is =
  let ty = is_type $ head is
   in prettyManyGoals is ty

prettyManyGoals :: [InScope] -> X.Type -> Doc HighlightGroup
prettyManyGoals is ty =
  hang 4 $ sep
    [ hsep $
        fmap (\i -> prettyVisibleName (is_in_scope i) $ is_refied_name i) is <> [":"]
    , prettyType ty
    ]

prettyGoal :: GoalInfo Text -> Doc HighlightGroup
prettyGoal (GoalInfo name ty) =
  hang 4 $ sep
    [ prettyName name <+> ":"
    , prettyType ty
    ]

prettyError :: Message -> Doc HighlightGroup
prettyError (Message msg) =
  let (hdr, body) = fmap (T.drop 1) $ T.break (== '\n') msg in
  vcat [ annotate CornelisError (pretty hdr) , pretty body ]
