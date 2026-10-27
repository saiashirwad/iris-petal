-- | A `Definition` names an effect and says how to run it. Issue one from
-- | `update` with `Petal.perform definition args toMessage`.
-- |
-- | The message function belongs to the issued `Command`, not the definition,
-- | so it can close over anything the caller knows (a cache key, a
-- | generation). Tests resolve a command with a plain `result` and never see
-- | that captured state.
module Petal.Command (module Internal) where

import Petal.Command.Internal (Command, Definition, define) as Internal
