module Main where

import FireSimulator.Simulator
import FireSimulator.Types
import FireSimulator.Information
import FireSimulator.Coordinate
import qualified Data.Vector as V
import Automata


main :: IO ()
main = do
    let sim = runSimulation 0 (Information 0 0) 10000 (defaultCoordinates 50)
    putStrLn $ "Steps: " ++ show (fst sim)
