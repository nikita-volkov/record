module Record where

import BasePrelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Record.Field


record :: QuasiQuoter
record =
  QuasiQuoter
    (const $ fail "Expression context is not supported")
    (const $ fail "Pattern context is not supported")
    (const $ fail "Type context is not supported")
    (const $ fail "Declaration context is not supported")

lens :: QuasiQuoter
lens =
  undefined
