-- | This module is not re-exported through GCB.Types.
module GCB.Types.Bar where

import GCB.Prelude

data Bar = Bar Int

mkBar :: Int -> Bar
mkBar = Bar

getBar :: Bar -> Int
getBar (Bar int) = int
