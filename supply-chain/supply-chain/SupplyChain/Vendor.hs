-- | Description: a /vendor/ responds to requests, makes requests, and
--                performs actions

module SupplyChain.Vendor
  (
    {- * Type -} Vendor (Vendor, handle),
    {- * Connection -} (>->), id,
    {- * Running -} run, eval,
    {- * Alteration -} alter,
    {- ** Some simple vendors -}
        function, action, absurd, map, forever,
  )
  where

import SupplyChain.Core.Connect ((>->))
import SupplyChain.Core.Job (Job)
import SupplyChain.Core.Referral (Referral (..))
import SupplyChain.Core.Vendor (Vendor (..), run, eval, alter)

import qualified SupplyChain.Core.Job as Job

import Control.Applicative (pure)
import Data.Function ((.))
import Data.Functor ((<&>))
import Data.Functor.Const (Const)
import Data.Void (Void)

absurd :: Vendor up (Const Void) action
absurd = Vendor (\case{})

-- | The identity for '(>->)'; does nothing at all
id :: Vendor i i action
id = forever Job.order

{-| A simple stateless vendor that responds to each
    request by applying a pure function -}
function :: (forall response. down response -> response)
    -> Vendor up down action
function f = forever (pure . f)

{-| A simple stateless vendor that responds to each
    request by applying an effectful function -}
action :: (forall response. down response -> action response)
    -> Vendor up down action
action f = forever (Job.perform . f)

map :: (forall x. down x -> up x) -> Vendor up down action
map f = forever (Job.order . f)

forever :: (forall x. down x -> Job up action x) -> Vendor up down action
forever f = go where go = Vendor (\x -> f x <&> (`Referral` go))
