-- | Description: a /referral/ consists of a product and a new vendor

module SupplyChain.Referral
  (
    {- * Type -} Referral (Referral, product, next),
    {- * Alteration -} alter,
  )
  where

import SupplyChain.Core.Referral (alter, Referral(..))
