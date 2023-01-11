-- | Functions for modifying requests and actions

module SupplyChain.Alter
  (
    {- * General -} job, vendor, request, perform,
    {- * Simplified -} job', vendor', request', perform',
  )
  where

import SupplyChain.Core.Effect (Effect)
import SupplyChain.Core.Job (Job)
import SupplyChain.Core.Vendor (Vendor (..))

import qualified SupplyChain.Core.Effect as Effect
import qualified SupplyChain.Core.Job as Job
import qualified SupplyChain.Core.Vendor as Vendor

import Data.Function ((.))

job :: (forall x. Effect up action x -> Job up' action' x)
    -- ^ Transformation applied to each effect that the job evokes
    -> Job up action product -> Job up' action' product
job = Job.alter

job' :: (forall x. Effect up action x -> Effect up' action' x)
    -- ^ Transformation applied to each effect that the job evokes
    -> Job up action product -> Job up' action' product
job' f = Job.alter (Job.effect . f)

vendor :: (forall x. Effect up action x -> Job up' action' x)
    -- ^ Transformation applied to each effect that the vendor evokes
    -> Vendor up down action -> Vendor up' down action'
vendor = Vendor.alter

vendor' :: (forall x. Effect up action x -> Effect up' action' x)
    -- ^ Transformation applied to each effect that the vendor evokes
    -> Vendor up down action -> Vendor up' down action'
vendor' f = Vendor.alter (Job.effect . f)

request :: (up product -> Effect up' action product)
    -- ^ Transformation applied if the effect is a request
    -> Effect up action product -> Effect up' action product
request = Effect.alterRequest

request' :: (up product -> up' product)
    -- ^ Transformation applied if the effect is a request
    -> Effect up action product -> Effect up' action product
request' f = Effect.alterRequest (Effect.Request . f)

perform :: (action product -> Effect up action' product)
    -- ^ Transformation applied if the effect is an action
    -> Effect up action product -> Effect up action' product
perform = Effect.alterPerform

perform' :: (action product -> action' product)
    -- ^ Transformation applied if the effect is an action
    -> Effect up action product -> Effect up action' product
perform' f = Effect.alterPerform (Effect.Perform . f)
