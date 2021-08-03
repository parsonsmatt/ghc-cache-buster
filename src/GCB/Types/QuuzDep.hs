{-# language QuasiQuotes #-}

module GCB.Types.QuuzDep where

import GCB.Prelude

import GCB.Types.Quuz

data QuuzDep = QuuzDep
    { quuzDepQuuz :: Quuz
    , quuzDepInt :: Int
    }

quuzDep :: QuuzDep
quuzDep = QuuzDep
    { quuzDepQuuz =
        let Right q = mkQuuz "asdfasdf"
         in q
    , quuzDepInt =
        3
    }
