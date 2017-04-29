{-# LANGUAGE TemplateHaskell            #-}
module Database.Types where


import Data.Aeson.TH
import Database.Models
import Database.Persist


data IncendioJSON = IncendioJSON
    { incendio :: Entity Incendio
    , coordenadas :: [Entity Coordenada]
    , satelite :: Maybe (Entity Satelite)
    , reportes :: [Entity Reporte]
    } deriving Show
$(deriveJSON defaultOptions ''IncendioJSON)
