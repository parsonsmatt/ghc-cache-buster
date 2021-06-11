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
  where x = 2 :: Int

-- wtf

-- addingThingFooExplicit = 3
