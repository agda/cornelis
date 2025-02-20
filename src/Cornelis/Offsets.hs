{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

-- | Strongly typed indices and offsets.
--
-- The goal of this module is to make it as easy as possible to keep track
-- of the various indexing schemes used by functions in the nvim API.
--
-- The core abstraction is the type 'Index' tagged with the sort of things it
-- is indexing (@Byte@, @CodePoint@, @Line@) and whether things are 0-indexed
-- or 1-indexed.
--
-- Two constructors and two destructors are provided: 'toZeroIndexed',
-- 'toOneIndexed', 'fromZeroIndexed', and 'fromOneIndexed'.  They should only
-- be used to make external API calls, to unwrap input indices and to wrap
-- output indices. The names of those functions are self-documenting, indicating
-- the indexing scheme used by every index that goes in and out of the external
-- API.
--
-- Within Cornelis, indices remain typed at all times, using dedicated functions
-- to convert between 0/1-indexing ('zeroIndex', 'oneIndex') and between
-- @Byte@ and @CodePoint@ indexing ('toByte', 'fromByte').
--
-- Usually, indices are relative to a common origin (beginning of the same buffer
-- or line), so it doesn't make sense to add them. There is a separate type of
-- 'Offset' which can be added to indices using the operator @('.+')@.
-- And @('.-.')@ gives the offset between two indices.
--
-- @
-- i :: Index 'Byte 'ZeroIndexed
-- i .+ Offset 42 :: Index 'Byte 'ZeroIndexed
-- @
--
-- Types of 'Pos'isitions (pairs of line and column indices) and 'Interval's
-- (pairs of positions or indices) are also provided, and should be used
-- as much as possible to reduce the likelihood of mixing up indices.
--
-- When talking about 'Pos', "(i,j)-indexed" means "i-indexed lines, j-indexed
-- columns".
--
-- Agda's indexing scheme (codepoints, (1,1)-indexed) is the preferred one
-- (0- vs 1-indexing is heavily checked, so it doesn't matter much which
-- we choose; codepoint indexing is preferred for manipulating unicode text
-- (fewer invalid states than byte indexing)).
--
-- A secondary indexing scheme is bytes, (0,0)-indexed, used as a unified
-- low-level representation right before talking to the nvim API.
module Cornelis.Offsets
  ( Index()
  , Indexing(..)
  , Unit(..)
  , Offset(..)
  , Pos(..)
  , Interval(..)
  , LineNumber
  , AgdaIndex
  , AgdaOffset
  , AgdaPos
  , AgdaInterval
  , VimIndex
  , VimOffset
  , VimPos
  , VimInterval
  , toZeroIndexed
  , toOneIndexed
  , fromZeroIndexed
  , fromOneIndexed
  , zeroIndex
  , oneIndex
  , incIndex
  , (.+)
  , (.-.)
  , offsetPlus
  , textToBytes
  , charToBytes
  , toBytes
  , fromBytes
  , containsPoint
  , addCol
  ) where

import           Data.Aeson (FromJSON)
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)
import           Data.Monoid (Sum(..))
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           GHC.Show (showSpace)
import           Prettyprinter (Pretty)
import qualified DiffLoc as D

-- | Indexing scheme: whether the first index is zero or one.
data Indexing = OneIndexed | ZeroIndexed

-- | What are we counting?
data Unit = Byte | CodePoint | Line

-- | The constructor is hidden, use 'toZeroIndexed' and 'toOneIndexed' to construct it,
-- and 'fromZeroIndexed' and 'fromOneIndexed' to destruct it.
newtype Index (e :: Unit) (i :: Indexing) = Index Int
  deriving newtype (Eq, Ord, Show, Read, FromJSON, Pretty)

type role Index nominal nominal

-- | It doesn't seem worth the trouble to hide this constructor.
newtype Offset (e :: Unit) = Offset Int
  deriving newtype (Eq, Ord, Show, Read, FromJSON, Pretty)
  deriving (Semigroup, Monoid) via Sum Int

type role Offset nominal

-- | Position in a text file as line-column numbers. This type is indexed by
-- the units of the columns (@Byte@ or @CodePoint@) and by the indexing scheme
-- of lines and columns.
data Pos e i j = Pos
  { p_line :: Index 'Line i
  , p_col :: Index e j
  } deriving (Eq, Ord, Generic)

instance Show (Pos e i j) where
  showsPrec n (Pos l c) =
    showParen (n >= 11) $ showString "Pn () 0 " . showsPrec 11 l . showSpace . showsPrec 11 c

data Interval p = Interval { iStart, iEnd :: !p }
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic)

instance Show p => Show (Interval p) where
  showsPrec n (Interval s e) =
    showParen (n >= 11) $ showString "Interval () " . showsPrec 11 s . showSpace . showsPrec 11 e

-- Common specializations

type LineNumber = Index 'Line

type AgdaIndex = Index 'CodePoint 'OneIndexed
type AgdaOffset = Offset 'CodePoint
type AgdaPos = Pos 'CodePoint 'OneIndexed 'OneIndexed
type AgdaInterval = Interval AgdaPos

type VimIndex = Index 'Byte 'ZeroIndexed
type VimOffset = Offset 'Byte
type VimPos = Pos 'Byte 'ZeroIndexed 'ZeroIndexed
type VimInterval = Interval VimPos

-- To pass indices to and from external sources.

-- | Mark a raw index as zero-indexed.
toZeroIndexed :: Integral a => a -> Index e 'ZeroIndexed
toZeroIndexed a = Index (fromIntegral a)

-- | Mark a raw index as one-indexed.
toOneIndexed :: Integral a => a -> Index e 'OneIndexed
toOneIndexed a = Index (fromIntegral a)

-- | Unwrap a raw zero-indexed index.
fromZeroIndexed :: Num a => Index e 'ZeroIndexed -> a
fromZeroIndexed (Index a) = fromIntegral a

-- | Unwrap a raw zero-indexed index.
fromOneIndexed :: Num a => Index e 'OneIndexed -> a
fromOneIndexed (Index a) = fromIntegral a

-- | Convert from one- to zero-indexed.
zeroIndex :: Index e 'OneIndexed -> Index e 'ZeroIndexed
zeroIndex (Index i) = Index (i - 1)

-- | Convert from zero- to one-indexed.
oneIndex :: Index e 'ZeroIndexed -> Index e 'OneIndexed
oneIndex (Index i) = Index (i + 1)

-- | Increment index.
incIndex :: Index e i -> Index e i
incIndex (Index i) = Index (i + 1)

-- | Add an offset to an index.
(.+) :: Index e i -> Offset e -> Index e i
Index i .+ Offset n = Index (i + n)

(.-.) :: Index e i -> Index e i -> Offset e
Index i .-. Index j = Offset (i - j)

--

offsetPlus :: Offset a -> Offset a -> Offset a
offsetPlus = coerce $ (+) @Int

--

containsPoint :: Ord p => Interval p -> p -> Bool
containsPoint (Interval s e) p = s <= p && p < e

-- | Number of bytes in a 'T.Text'.
textToBytes :: T.Text -> Int
textToBytes t = BS.length (encodeUtf8 t)

-- | Number of bytes in a 'Char'.
charToBytes :: Char -> Int
charToBytes c = textToBytes (T.singleton c)

------------------------------------------------------------------------------
-- | Convert a character-based index into a byte-indexed one
toBytes :: T.Text -> Index 'CodePoint 'ZeroIndexed -> Index 'Byte 'ZeroIndexed
toBytes s (Index i) = Index $ textToBytes $ T.take (fromIntegral i) s

------------------------------------------------------------------------------
-- | Convert a byte-based index into a character-indexed one.
fromBytes :: HasCallStack => T.Text -> Index 'Byte 'ZeroIndexed -> Index 'CodePoint 'ZeroIndexed
fromBytes t (Index i) | i < 0 = error $ "from bytes underflow" <> show (t, i)
fromBytes _ (Index 0) = Index 0
fromBytes t (Index i) | Just (c, str) <- T.uncons t =
  let diff = BS.length $ encodeUtf8 $ T.singleton c
   in case i - diff >= 0 of
        True -> Index $ 1 + coerce (fromBytes str (Index (i - diff)))
        -- We ran out of bytes in the middle of a multibyte character. Just
        -- return the one we're on, and don't underflow!
        False -> Index 0
fromBytes t i = error $ "missing bytes: " <> show (t, i)

addCol :: Pos e i j -> Offset e -> Pos e i j
addCol (Pos l c) dc = Pos l (c .+ dc)

-- | Ordered monoid action of offsets on indices.
instance D.Amor (Index e i) where
  type Trans (Index e i) = Offset e
  (.+) = (Cornelis.Offsets..+)
  i .-.? j | i >= j = Just (i .-. j)
           | otherwise = Nothing

-- | The zero in zero-indexing.
instance D.Origin (Index e 'ZeroIndexed) where
  origin = toZeroIndexed (0 :: Int)
