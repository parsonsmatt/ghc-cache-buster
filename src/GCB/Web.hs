module GCB.Web where

import GCB.Types ()
import GCB.Types.Bar
import GCB.App

web :: IO ()
web = do
    putStrLn "starting webserver"
    let f = mkBar 3
    app
