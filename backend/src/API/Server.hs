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
import Database.Types



type ServerAPI =
         "test" :> Get '[JSON] Text
    :<|> "incendio" :> "area" :> Get '[JSON] IncendioArea



serverAPI :: Proxy ServerAPI
serverAPI = Proxy

server :: Server ServerAPI
server =
    test :<|>
    incendio'


incendio' = undefined

test = return "Hola mundo"

app :: Application
app = serve serverAPI server
