-- | Description: a /job/ makes requests, performs actions, and returns

module SupplyChain.Job
  (
    {- * Type -} Job,
    {- * Construction -} perform, order, effect,
    {- * Running -} run, eval,
    {- * Alteration -} alter,
  )
  where

import SupplyChain.Core.Job (Job, alter, effect, eval, order, perform, run)
