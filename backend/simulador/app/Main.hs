module Main where

import FireSimulator.Simulator
import FireSimulator.Types
import FireSimulator.Information
import FireSimulator.Coordinate
import qualified Data.Vector as V
import Automata


main :: IO ()
main = do
    let x = snd $ runSimulation 0 (Information 2 2) 250 (defaultCoordinates 50)
    {-writeJSON "salida.json" [x]-}
    writePoligonoJSON "poligono.json" (poligono x)
