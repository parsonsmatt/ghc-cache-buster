{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module GCB.Explicit where

import GCB.Types.Foo (mkFoo)
import GCB.Types.FooExplicit (mkFooExplicit)
import GCB.Types.Quuz (compileQuuz, blargh)

app :: IO ()
app = do
    putStrLn "Hello, World!"
    let x = [compileQuuz|asdf|]
    let foo = mkFoo "asdf"
    putStrLn "Goodbye, World!"

blargh
