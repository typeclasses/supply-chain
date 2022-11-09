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
import SupplyChain.Core.Job (Job)

import qualified SupplyChain.Core.Job as Job

import Control.Monad (Monad)
import Data.Function ((.))
import Data.Functor.Const (Const)
import Data.Void (Void)

alter :: (forall x. Effect up action x -> Job up' action' x)
    -- ^ Transformation applied to each effect that the job evokes
    -> Job up action product -> Job up' action' product
alter f = Job.alter f

alter' :: (forall x. Effect up action x -> Effect up' action' x)
    -- ^ Transformation applied to each effect that the job evokes
    -> Job up action product -> Job up' action' product
alter' f = Job.alter (effect . f)

perform :: action product -- ^ Action
    -> Job up action product -- ^ Job
perform = Job.perform

order :: up product -- ^ Request
    -> Job up action product -- ^ Job
order = Job.order

effect :: Effect up action product -- ^ Effect
    -> Job up action product -- ^ Job
effect = Job.effect

{-| Run a job in its action context

   The job must not make requests, so its upstream interface
   is @Const Void@. -}
run :: Monad action =>
    Job (Const Void) action product -- ^ Job
    -> action product -- ^ Action
run = Job.run

{-| Evaluate a job with no context

    The job must evokes neither request nor actions, so both
    its upstream and action contexts are @Const Void@. -}
eval :: Job (Const Void) (Const Void) product -- ^ Job
    -> product -- ^ Result
eval = Job.eval
