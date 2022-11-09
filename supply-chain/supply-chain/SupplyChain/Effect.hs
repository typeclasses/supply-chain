-- | Description: an /effect/ is either /request/ or /perform/

module SupplyChain.Effect
  (
    {- * Type -} Effect (Request, Perform),
    {- * Alteration -} alterRequest, alterPerform,
  )
  where

import SupplyChain.Core.Effect (Effect (Request, Perform), alterPerform, alterRequest)
