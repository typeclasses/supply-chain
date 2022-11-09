-- | Description: an /effect/ is either /request/ or /perform/

module SupplyChain.Effect
  (
    {- * Type -} Effect (Request, Perform),
    {- * General alteration -} alterRequest, alterPerform,
    {- * Simplified alteration -} alterRequest', alterPerform',
  )
  where

import SupplyChain.Core.Effect (Effect (Request, Perform))

import qualified SupplyChain.Core.Effect as Effect

import Data.Function ((.))

alterRequest :: (up product -> Effect up' action product)
    -- ^ Transformation applied if the effect is a request
    -> Effect up action product -> Effect up' action product
alterRequest = Effect.alterRequest

alterRequest' :: (up product -> up' product)
    -- ^ Transformation applied if the effect is a request
    -> Effect up action product -> Effect up' action product
alterRequest' f = Effect.alterRequest (Effect.Request . f)

alterPerform :: (action product -> Effect up action' product)
    -- ^ Transformation applied if the effect is an action
    -> Effect up action product -> Effect up action' product
alterPerform = Effect.alterPerform

alterPerform' :: (action product -> action' product)
    -- ^ Transformation applied if the effect is an action
    -> Effect up action product -> Effect up action' product
alterPerform' f = Effect.alterPerform (Effect.Perform . f)
