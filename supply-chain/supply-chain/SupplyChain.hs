module SupplyChain
  (

    {- * Modules -} {- $modules -}
    {- * Job type -} Job,
    {- * Making jobs -} order, perform,
    {- * Running jobs -} run, eval,
    {- * Vendor type -} Vendor (Vendor, handle), Referral (Referral),
    {- * Vendor connection -} (>->),
    {- * Vendor-job connection -} (>-), (>+),
    {- * Vendor/job conversion -} once, loop, loop', Unit (Unit),
  )
  where

import SupplyChain.Job (Job, order, perform, run, eval)
import SupplyChain.JobAndVendor (loop, loop', once, (>-), (>+))
import SupplyChain.Referral (Referral (Referral))
import SupplyChain.Unit (Unit (Unit))
import SupplyChain.Vendor (Vendor (Vendor, handle), (>->))

{- $modules

* "SupplyChain.Alter" — Functions for modifying requests and actions
* "SupplyChain.Effect" — An /effect/ is either /request/ or /perform/
* "SupplyChain.Job" — A /job/ makes requests, performs actions, and returns
* "SupplyChain.JobAndVendor" — /Job/ + /vendor/
* "SupplyChain.Referral" — A /referral/ consists of a product and a new vendor
* "SupplyChain.Unit" — /Unit/ is a simple interface with one request and a fixed response type
* "SupplyChain.Vendor" — A /vendor/ responds to requests, makes requests, and performs actions

-}
