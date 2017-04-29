module Main where

import qualified API.Server as Server
import Network.Wai
import Network.Wai.Handler.Warp


main :: IO ()
main = run 8080 Server.app
