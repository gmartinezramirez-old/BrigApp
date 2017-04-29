{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module API.Server where


import Servant
import Data.Proxy
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)



type ServerAPI = "test" :> Get '[JSON] Text



serverAPI :: Proxy ServerAPI
serverAPI = Proxy

server :: Server ServerAPI
server = do
    liftIO $ putStrLn "Hola mundo"
    return "Hola mundo"

app :: Application
app = serve serverAPI server
