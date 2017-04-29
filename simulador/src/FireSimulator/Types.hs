{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module FireSimulator.Types where

import Data.Aeson.TH
import Text.Printf



data Coordinate = Coordinate
    { lat :: !Double
    , lng :: !Double
    } deriving (Show)
$(deriveJSON defaultOptions ''Coordinate)


data WindDirection =
      WindN
    | WindS
    | WindW
    | WindE
    | WindNW
    | WindNE
    | WindSW
    | WindSE
    deriving Show
$(deriveJSON defaultOptions ''WindDirection)



data Information = Information
    { uwind :: !Double
    , vwind :: !Double
    } deriving Show
$(deriveJSON defaultOptions ''Information)

data Cell = Cell
    { state       :: !Double
    , burned      :: !Bool
    , position    :: !Coordinate
    , information :: !Information
    }
$(deriveJSON defaultOptions ''Cell)

instance Show Cell where
    show Cell{..}
        | state > 0 = printf "\x1B[31m%.2f\x1B[0m" state
        | otherwise = printf "%.2f" state
