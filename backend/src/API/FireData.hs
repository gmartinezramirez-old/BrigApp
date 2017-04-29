{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module API.FireData where

import Network.HTTP
import Data.Csv
import GHC.Generics
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad (liftM)
import Data.Convertible
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.Stream (Result)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Models
import Database.Persist
import Data.Time.Clock (UTCTime)
import Control.Monad.IO.Class
import Data.Monoid
import Data.Time.Format


data FireData = FireData


data ModisData = ModisData
    { modisLat        :: !Double
    , modisLng        :: !Double
    , modisBrightness :: !Double
    , modisDate       :: !Text
    , modisTime       :: !Text
    , modisConfidence :: !Double
    }
    deriving (Show, Generic)



instance FromNamedRecord ModisData where
    parseNamedRecord x =
        ModisData 
        <$> x .: "latitude" 
        <*> x .: "longitude"
        <*> x .: "brightness"
        <*> x .: "acq_date"
        <*> x .: "acq_time"
        <*> x .: "confidence"




data ViirsData = ViirsData
    { viirsLat        :: !Double
    , viirsLng        :: !Double
    , viirsBrightness :: !Double
    , viirsDate       :: !Text
    , viirsTime       :: !Text
    , viirsConfidence :: !Text
    }
    deriving (Show, Generic)



instance FromNamedRecord ViirsData where
    parseNamedRecord x =
        ViirsData 
        <$> x .: "latitude" 
        <*> x .: "longitude"
        <*> x .: "bright_ti4"
        <*> x .: "acq_date"
        <*> x .: "acq_time"
        <*> x .: "confidence"


loadMobis :: FilePath -> IO (Vector ModisData)
loadMobis file = do
    fs <- BS.readFile file
    case decodeByName fs of
        Left err -> error err
        Right (_, csv) -> return csv


loadViirs :: FilePath -> IO (Vector ViirsData)
loadViirs file = do
    fs <- BS.readFile file
    case decodeByName fs of
        Left err -> error err
        Right (_, csv) -> return csv


storeMobis :: Vector ModisData -> Query ()
storeMobis = mapM_ $ \ModisData{..} ->
    if modisLat < -17.435 && modisLng > -76.904 && modisLng < -65.259
        then do
            time :: UTCTime <- parseTimeM True defaultTimeLocale "%0Y-%0m-%0d %0H%0M" (T.unpack $ modisDate <> " " <> modisTime)
            satelite <- insert $ Satelite modisLat modisLng "MODIS" modisBrightness time (T.pack $ show modisConfidence)
            incendio <- insert $ Incendio True False (Just satelite)
            coordenada <- insert $ Coordenada incendio modisLat modisLng
            return ()
        else return ()
