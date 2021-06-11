module GCB.Web where

import GCB.Types (Foo)
import GCB.App

web :: IO ()
web = do
    putStrLn "starting webserver"
    app
