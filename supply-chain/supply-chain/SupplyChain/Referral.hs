-- | Description: a /referral/ consists of a product and a new vendor

module SupplyChain.Referral
  (
    {- * Type -} Referral (Referral, product, next),
    {- * Alteration -} alter, alter',
  )
  where

import SupplyChain.Core.Effect (Effect)
import SupplyChain.Core.Job (Job, effect)
import SupplyChain.Core.Referral (Referral (Referral, product, next))

import qualified SupplyChain.Core.Referral as Referral

import Data.Function ((.))

alter :: (forall x. Effect up action x -> Job up' action' x)
    -- ^ Transformation applied to each effect that the 'next' vendor evokes
    -> Referral up down action product -> Referral up' down action' product
alter = Referral.alter

alter' :: (forall x. Effect up action x -> Effect up' action' x)
    -- ^ Transformation applied to each effect that the 'next' vendor evokes
    -> Referral up down action product -> Referral up' down action' product
alter' f = Referral.alter (effect . f)
