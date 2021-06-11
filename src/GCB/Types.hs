module GCB.Types
    ( DefinedInTypes(..)
    , module X
    ) where

import GCB.Prelude

import GCB.Types.Foo as X
import GCB.Types.Quuz as X

data DefinedInTypes = DefinedInTypes Int Char

-- addingThingT = 1
