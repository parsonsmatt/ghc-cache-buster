{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module GCB.App where

import GCB.Prelude

import GCB.Types

app :: IO ()
app = do
    putStrLn "Hello, World!"
    let x = [compileQuuz|asdf|]
    putStrLn "Goodbye, World!"

blargh