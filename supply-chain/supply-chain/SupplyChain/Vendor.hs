-- | Description: a /vendor/ responds to requests, makes requests, and
--                performs actions

module SupplyChain.Vendor
  (
    {- * Type -} Vendor (Vendor, handle),
    {- * Connection -} (>->), id,
    {- * Running -} run, eval,
    {- ** Some simple vendors -} function, action, map, absurd,
    {- * Alteration -} alter, alter',
  )
  where

import SupplyChain.Core.Effect (Effect)
import SupplyChain.Core.Job (Job, effect)
import SupplyChain.Core.Referral (Referral (..))
import SupplyChain.Core.Vendor (Vendor (..))
import SupplyChain.JobAndVendor (loop')

import qualified SupplyChain.Core.Connect as Connect
import qualified SupplyChain.Core.Job as Job
import qualified SupplyChain.Core.Vendor as Vendor

import Control.Applicative (pure)
import Control.Monad (Monad)
import Data.Function ((.))
import Data.Functor.Const (Const)
import Data.Void (Void)

-- | Connect two vendors; the first interprets requests made by the second
(>->) :: Vendor up middle action -- ^ Upstream
    -> Vendor middle down action -- ^ Downstream
    -> Vendor up down action
(>->) = (Connect.>->)

run :: Monad action => Vendor (Const Void) down action -- ^ Vendor
    -> down product -- ^ Request
    -> action (Referral (Const Void) down action product)
run = Vendor.run

eval :: Vendor (Const Void) down (Const Void) -- ^ Vendor
    -> down product -- ^ Request
    -> Referral (Const Void) down (Const Void) product
eval = Vendor.eval

-- | Vendor that never responds to any requests
absurd :: Vendor up (Const Void) action
absurd = Vendor (\case{})

-- | The identity for '(>->)'; does nothing at all
id :: Vendor i i action
id = loop' Job.order

{-| A simple stateless vendor that responds to each
    request by applying a pure function -}
function :: (forall response. down response -> response)
    -> Vendor up down action
function f = loop' (pure . f)

{-| A simple stateless vendor that responds to each
    request by applying an effectful function -}
action :: (forall response. down response -> action response)
    -> Vendor up down action
action f = loop' (Job.perform . f)

{-| A vendor that applies a transformation to each request
    and then simply forwards it upstream. -}
map :: (forall x. down x -> up x) -> Vendor up down action
map f = loop' (Job.order . f)

alter :: (forall x. Effect up action x -> Job up' action' x)
    -- ^ Transformation applied to each effect that the vendor evokes
    -> Vendor up down action -> Vendor up' down action'
alter = Vendor.alter

alter' :: (forall x. Effect up action x -> Effect up' action' x)
    -- ^ Transformation applied to each effect that the vendor evokes
    -> Vendor up down action -> Vendor up' down action'
alter' f = Vendor.alter (effect . f)
