module Moe.App.Monad
  ( MoeM,
  )
where

import Effectful
import RequireCallStack (RequireCallStack)

type MoeM es a = RequireCallStack => Eff es a
