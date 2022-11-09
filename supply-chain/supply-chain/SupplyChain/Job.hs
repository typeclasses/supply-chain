-- | Description: a /job/ makes requests, performs actions, and returns

module SupplyChain.Job
  (
    {- * Type -} Job,
    {- * Construction -} perform, order, effect,
    {- * Running -} run, eval,
    {- * Alteration -} alter, alter',
  )
  where

import SupplyChain.Core.Effect (Effect)
import SupplyChain.Core.Job (Job, effect, eval, order, perform, run)

import qualified SupplyChain.Core.Job as Job

import Data.Function ((.))

alter :: (forall x. Effect up action x -> Job up' action' x)
    -- ^ Transformation applied to each effect that the job evokes
    -> Job up action product -> Job up' action' product
alter f = Job.alter f

alter' :: (forall x. Effect up action x -> Effect up' action' x)
    -- ^ Transformation applied to each effect that the job evokes
    -> Job up action product -> Job up' action' product
alter' f = Job.alter (effect . f)
