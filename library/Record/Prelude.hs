module Record.Prelude
( 
  module Exports,
)
where


-- * base-prelude
-------------------------
import BasePrelude as Exports hiding (left, right, isLeft, isRight)

-- * transformers
-------------------------
import Control.Monad.Trans.State.Strict as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Control.Monad.Trans.Reader as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Control.Monad.Trans.Writer as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Control.Monad.Trans.Maybe as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Control.Monad.Trans.Class as Exports
import Control.Monad.IO.Class as Exports
import Data.Functor.Identity as Exports

-- * text
-------------------------
import Data.Text as Exports (Text)

-- * unordered-containers
-------------------------
import Data.HashMap.Strict as Exports (HashMap)
