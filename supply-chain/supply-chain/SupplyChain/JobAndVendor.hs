-- | /Job/ + /vendor/

module SupplyChain.JobAndVendor
  (
    {- * Connection -} (>-), (>+),
    {- * Conversion -} once, loop, loop',
  )
  where

import SupplyChain.Core.Job (Job)
import SupplyChain.Core.Referral (Referral (Referral))
import SupplyChain.Core.Unit (Unit)
import SupplyChain.Core.Vendor (Vendor (Vendor, handle))

import qualified SupplyChain.Core.JobAndVendor as Core
import qualified SupplyChain.Core.Connect as Core

import Data.Functor ((<&>))

{-| Modify a job with a vendor that interprets its requests -}
(>-) :: Vendor up down action -- ^ Upstream
     -> Job down action product -- ^ Downstream
     -> Job up action product
(>-) = (Core.>-)

{-| Connect a vendor to a job, producing a job which returns both the product
    and a new version of the vendor.

    Use this function instead of '(>-)' if you need to attach a succession
    of jobs to one stateful vendor. -}
(>+) :: Vendor up down action -- ^ Upstream
     -> Job down action product -- ^ Downstream
     -> Job up action (Referral up down action product)
(>+) = (Core.>+)

loop :: Job up action product -- ^ Job
    -> Vendor up (Unit product) action -- ^ Vendor
loop = Core.loop

once :: Vendor up (Unit product) action -- ^ Vendor
    -> Job up action product -- ^ Job
once = Core.once

loop' :: (forall x. down x -> Job up action x) -- ^ Stateless handler
    -> Vendor up down action -- ^ Vendor
loop' f = go where go = Vendor{ handle = \x -> f x <&> (`Referral` go) }
