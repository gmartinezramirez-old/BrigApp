{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module FireSimulator.Types where

import Data.Aeson.TH
import Text.Printf
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BS
import Data.Aeson



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


data CoordenadaGraph = CoordenadaGraph
    { id         :: Int
    , points     :: [(Double, Double)]
    , value      :: Double
    , value_type :: Text
    , mode       :: Text
    }
$(deriveJSON defaultOptions ''CoordenadaGraph)


convertToGraph :: V.Vector (V.Vector Coordinate) -> V.Vector (V.Vector CoordenadaGraph)
convertToGraph = V.map (\v -> V.map (\Coordinate{..} -> CoordenadaGraph 1 [(lng, lat)] 20 "speed" "REPLACE") v)


writeJSONGraph :: FilePath -> V.Vector (V.Vector CoordenadaGraph) -> IO  ()
writeJSONGraph file coords = 
    let json = encode . concat . V.toList . V.map (\v -> V.toList v) $ coords
    in BS.writeFile file json
