{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
module API.Elevation where

import Data.Aeson hiding (Result)
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Data.Aeson.TH
import Data.Text (Text)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.List (foldl')
import Data.Monoid
import qualified Data.Text as T
import FireSimulator.Types
import qualified Data.Vector as V
import Database.Models
import Control.Monad
import Database.Persist
import Web.HttpApiData
import Data.Either



apiKey :: Text
apiKey = "AIzaSyAbGfZmXCACLmmJo1mKJtJbH_YA09SHYAY "

data Location = Location
    { lat :: Double
    , lng :: Double
    } deriving Show
$(deriveJSON defaultOptions ''Location)

data Result = Result
    { elevation :: Double
    , location :: Location
    } deriving Show
$(deriveJSON defaultOptions ''Result)

data Elevation = Elevation
    { results :: [Result]
    } deriving Show
$(deriveJSON defaultOptions ''Elevation)


data CoordenadaLista = CoordenadaLista
    { coordenadasLista :: [(Double, Double)]
    } deriving Show

instance ToHttpApiData CoordenadaLista where
    toUrlPiece (CoordenadaLista xs) = T.reverse . T.drop 1 . T.reverse $ foldl' (\acc (x, y) -> acc <> (T.pack $ show x) <> "," <> (T.pack $ show y) <> "|") "" xs
    


-- | Base URL to the version 2.8 of the API
baseUrl :: BaseUrl
baseUrl = BaseUrl Https "maps.googleapis.com" 443 "/maps/api/elevation"


type GoogleElevation = "json" :> QueryParam "locations" CoordenadaLista :> QueryParam "key" Text :> Get '[JSON] Elevation


elevation' :: Maybe CoordenadaLista -> Maybe Text -> ClientM Elevation

elevation' = client (Proxy :: Proxy GoogleElevation)


runElevation :: (Maybe Text -> ClientM a) -> IO (Either ServantError a)
runElevation endpoint = do
    manager <- newManager tlsManagerSettings
    res <- runClientM (endpoint (Just apiKey)) (ClientEnv manager baseUrl)
    case res of
      Left err -> return . Left $ err
      Right x -> return . Right $ x


byPiece :: CoordenadaLista -> IO [Either ServantError Elevation]
byPiece (CoordenadaLista xs) = do
    let l = take 10 xs
        n = drop 10 xs
    results <- runElevation (elevation' (Just (CoordenadaLista l)))
    next <- byPiece (CoordenadaLista n)
    return $ results : next


convert :: V.Vector (V.Vector Coordinate) -> CoordenadaLista
convert coords =
    let ls = concat . V.toList . V.map (\v -> V.toList v) $ coords
    in CoordenadaLista . map (\Coordinate{..} -> (lat, lng)) $ ls

storeElevation :: Either a Elevation -> IO ()
storeElevation (Right (Elevation xs)) = runDB $ forM_ xs $ \(Result elevation (Location lat lng)) -> do
    insert $ Elevacion lat lng elevation


