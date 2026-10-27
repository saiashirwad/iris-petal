-- | A `Definition` names an effect and says how to run it. Issue one from
-- | `update` with `Foldkit.perform definition args toMessage`.
-- |
-- | The message function belongs to the issued `Command`, not the definition,
-- | so it can close over anything the caller knows (a cache key, a
-- | generation). Tests resolve a command with a plain `result` and never see
-- | that captured state.
module Foldkit.Command (module Internal) where

import Foldkit.Command.Internal (Command, Definition, define) as Internal
