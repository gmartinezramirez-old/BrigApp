{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module API.OpenWeatherMap where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Data.Aeson.TH
import Data.Text (Text)
import Network.HTTP.Client.TLS (tlsManagerSettings)



-- | Base URL to the version 2.8 of the API
baseUrl :: BaseUrl
baseUrl = BaseUrl Http "api.openweathermap.org" 80 ""

data WeatherCountry = WeatherCountry
    { country :: Text
    , sunrise :: Int
    , sunset  :: Int
    } deriving Show
$(deriveJSON defaultOptions ''WeatherCountry)

data WeatherMain = WeatherMain
    { temp :: Double
    , humidity :: Double
    , pressure :: Double
    } deriving Show
$(deriveJSON defaultOptions ''WeatherMain)

data WeatherWind = WeatherWind
    { speed :: Double
    , deg   :: Double
    } deriving Show
$(deriveJSON defaultOptions ''WeatherWind)


data Weather = Weather
    { name :: Text            -- ^ El nombre del lugar local
    , sys  :: WeatherCountry  -- ^ Información del pais
    , main :: WeatherMain     -- ^ Datos principales de la API
    , wind :: WeatherWind
    } deriving Show
$(deriveJSON defaultOptions ''Weather)


type OpenWeatherMapAPI = "data" :> "2.5" :> "weather" :> QueryParam "lat" Double :> QueryParam "lon" Double :> QueryParam "appid" Text :> Get '[JSON] Weather


weather :: Maybe Double -> Maybe Double -> Maybe Text -> ClientM Weather

weather = client (Proxy :: Proxy OpenWeatherMapAPI)


runWeather :: (Maybe Text -> ClientM a) -> IO (Either ServantError a)
runWeather endpoint = do
    manager <- newManager tlsManagerSettings
    res <- runClientM (endpoint (Just "3afc7224baa0183aee699b65ae5af174")) (ClientEnv manager baseUrl)
    case res of
      Left err -> return . Left $ err
      Right x -> return . Right $ x
