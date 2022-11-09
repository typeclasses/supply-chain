module SupplyChain
  (

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
