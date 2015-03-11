module Record.Preprocessor.Rendering.Render where

import Record.Prelude
import qualified Record.Preprocessor.Position as Position
import qualified Record.Preprocessor.Parse as Parse
import qualified Record.Preprocessor.Rendering.MeasuredString as MeasuredString


type Render =
  StateT RenderState

type RenderState =
  (Position.Position, StringBuilder)

-- |
-- A list of strings in a reverse order.
-- Thus it is optimised for appending.
type StringBuilder =
  [String]


putString :: Monad m => String -> Render m ()
putString =
  putMeasuredString . MeasuredString.string

putSpace :: Monad m => Position.Position -> Render m ()
putSpace =
  putMeasuredString . MeasuredString.space

putMeasuredString :: Monad m => MeasuredString.MeasuredString -> Render m ()
putMeasuredString (o, c) =
  modify $ \(so, sb) -> (Position.add o so, c : sb)

cursorOffset :: Monad m => Render m Position.Position
cursorOffset =
  liftM fst get

-- |
-- Fills with whitespace until an offset.
fillWithSpaceUntil :: Monad m => Position.Position -> Render m ()
fillWithSpaceUntil targetOffset =
  do
    (currentOffset, _) <- get
    putSpace (max Position.zero (Position.subtract currentOffset targetOffset))


