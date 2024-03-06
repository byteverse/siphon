module Siphon.Encoding where

import Colonnade (Colonnade, Headed)
import Pipes (Pipe, yield)
import Siphon.Types

import qualified Colonnade.Encode as E
import qualified Pipes.Prelude as Pipes

row :: Siphon c -> Colonnade f a c -> a -> c
row (Siphon escape intercalate _ _) e =
  intercalate . E.row escape e

header :: Siphon c -> Colonnade Headed a c -> c
header (Siphon escape intercalate _ _) e =
  intercalate (E.header escape e)

pipe ::
  (Monad m) =>
  Siphon c ->
  Colonnade f a c ->
  Pipe a c m x
pipe siphon encoding = Pipes.map (row siphon encoding)

headedPipe ::
  (Monad m) =>
  Siphon c ->
  Colonnade Headed a c ->
  Pipe a c m x
headedPipe siphon encoding = do
  yield (header siphon encoding)
  pipe siphon encoding
