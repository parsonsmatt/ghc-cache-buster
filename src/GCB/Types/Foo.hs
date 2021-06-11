module GCB.Types.Foo where

data Foo = Foo String

mkFoo :: String -> Foo
mkFoo = Foo

getFoo :: Foo -> String
getFoo (Foo str) = str

-- addingThing = 3
