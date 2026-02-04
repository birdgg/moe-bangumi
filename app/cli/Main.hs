module Main (main) where

import Moe.Prelude
import Moe.Web.Server qualified as Server

main :: IO ()
main = Server.runMoe
