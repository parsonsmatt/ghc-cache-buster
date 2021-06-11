module GCB.Types.FooExplicit
    ( FooExplicit(..)
    , mkFooExplicit
    , getFooExplicit
    ) where

data FooExplicit = FooExplicit String

mkFooExplicit :: String -> FooExplicit
mkFooExplicit = FooExplicit

getFooExplicit :: FooExplicit -> String
getFooExplicit (FooExplicit str) = str

addingThingFooExplicit = 3
