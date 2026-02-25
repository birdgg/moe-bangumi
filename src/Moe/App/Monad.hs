module Moe.App.Monad
  ( MoeM,
  )
where

import Moe.Prelude
import RequireCallStack (RequireCallStack)

type MoeM es a = RequireCallStack => Eff es a
