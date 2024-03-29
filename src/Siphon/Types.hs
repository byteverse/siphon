{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Siphon.Types
  ( Siphon (..)
  , Indexed (..)
  , SiphonError (..)
  , RowError (..)
  , CellError (..)
  ) where

import Control.Exception (Exception)
import Data.Functor.Classes (Eq1, Show1, liftEq, liftShowsPrec)
import Data.Text (Text)
import Data.Vector (Vector)

data CellError = CellError
  { cellErrorColumn :: !Int
  , cellErrorContent :: !Text
  }
  deriving (Show, Read, Eq)

newtype Indexed a = Indexed
  { indexedIndex :: Int
  }
  deriving (Eq, Ord, Functor, Show, Read)

instance Show1 Indexed where
  liftShowsPrec _ _ p (Indexed i) s = showsPrec p i s

instance Eq1 Indexed where
  liftEq _ (Indexed i) (Indexed j) = i == j

data SiphonError = SiphonError
  { siphonErrorRow :: !Int
  , siphonErrorCause :: !RowError
  }
  deriving (Show, Read, Eq)

instance Exception SiphonError

data RowError
  = -- | Error occurred parsing the document into cells
    RowErrorParse
  | -- | Error decoding the content
    RowErrorDecode !(Vector CellError)
  | -- | Wrong number of cells in the row
    RowErrorSize !Int !Int
  | -- | Three parts:
    --   (a) Multiple header cells matched the same expected cell,
    --   (b) Headers that were missing,
    --   (c) Missing headers that were lambdas. They cannot be
    --   shown so instead their positions in the 'Siphon' are given.
    RowErrorHeaders !(Vector (Vector CellError)) !(Vector Text) !(Vector Int)
  | -- | Not enough cells in header, expected, actual
    RowErrorHeaderSize !Int !Int
  | -- | Error decoding unicode content, column number
    RowErrorMalformed !Int
  deriving (Show, Read, Eq)

{- | This just actually a specialization of the free applicative.
  Check out @Control.Applicative.Free@ in the @free@ library to
  learn more about this. The meanings of the fields are documented
  slightly more in the source code. Unfortunately, haddock does not
  play nicely with GADTs.
-}
data Siphon f c a where
  SiphonPure ::
    !a -> -- function
    Siphon f c a
  SiphonAp ::
    !(f c) -> -- header
    !(c -> Maybe a) -> -- decoding function
    !(Siphon f c (a -> b)) -> -- next decoding
    Siphon f c b

instance Functor (Siphon f c) where
  fmap f (SiphonPure a) = SiphonPure (f a)
  fmap f (SiphonAp h c apNext) = SiphonAp h c ((f .) <$> apNext)

instance Applicative (Siphon f c) where
  pure = SiphonPure
  SiphonPure f <*> y = fmap f y
  SiphonAp h c y <*> z = SiphonAp h c (flip <$> y <*> z)
