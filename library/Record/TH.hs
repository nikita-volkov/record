module Record.TH where

import BasePrelude hiding (Proxy)
import Data.Functor.Identity
import GHC.TypeLits
import Foreign.Storable
import Foreign.Ptr (plusPtr)
import Language.Haskell.TH


recordTypeDec :: Bool -> Int -> Dec
recordTypeDec strict arity =
  DataD [] typeName varBndrs [NormalC typeName conTypes] []
  where
    typeName =
      mkName $ prefix <> "Record" <> show arity
      where
        prefix =
          if strict then "Strict" else "Lazy"
    varBndrs =
      do
        i <- [1 .. arity]
        let
          n = KindedTV (mkName ("n" <> show i)) (ConT ''Symbol)
          v = PlainTV (mkName ("v" <> show i))
          in [n, v]
    conTypes =
      do
        i <- [1 .. arity]
        return $ (,) strictness (VarT (mkName ("v" <> show i)))
      where
        strictness =
          if strict then IsStrict else NotStrict

