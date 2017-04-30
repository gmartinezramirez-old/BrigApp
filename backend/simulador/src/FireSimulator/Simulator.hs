{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module FireSimulator.Simulator where


import Automata
import FireSimulator.Types
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.List (foldl')
import FireSimulator.Information
import Debug.Trace
import Data.List


-- | TODO: Por ahora usamos la misma información en todas las celdas, una
-- generalización que debería funcionar para incendios pequeños
initialize :: Information -> V.Vector (V.Vector Coordinate) -> Grid Cell
initialize info coords = initial iMax jMax create
    where
        create :: Int -> Int -> Cell
        create i j
            | i == iMiddle && j == jMiddle = Cell 1 True (getCoord i j) info
            | otherwise = Cell 0 False (getCoord i j) info

        iMiddle = (iMax `div` 2)
        jMiddle = (jMax `div` 2)

        iMax = length coords 
        jMax = if iMax > 0 then length (coords V.! 0) else 0

        getCoord :: Int -> Int -> Coordinate
        getCoord i j = coords V.! i V.! j



rule :: SubGrid Cell -> Cell -> Cell
rule grid c@(Cell _ True _ _) = c
rule grid (Cell current False coord inf) = Cell nextState False coord inf
    where
        wind = windGrid grid
        terrain = terrainGrid grid

        -- Primera 
        sum1 = foldl' (\acc t -> acc + generateMatrixMul t) 0.0
            [ (1, 0)
            , (2, 1)
            , (0, 1)
            , (1, 2)
            ]

        sum2 = foldl' (\acc t -> acc + generateMatrixMul t) 0.0
            [ (2, 0)
            , (0, 0)
            , (2, 2)
            , (0, 2)
            ]

        nextState = current + sum1 + 0.785*sum2

        generateMatrixMul :: (Int, Int) -> Double
        generateMatrixMul (i, j) = (mIndexDouble i j wind) * (mIndexDouble i j terrain) * (state (mIndexCell i j grid))


mIndexDouble :: Int -> Int -> Vector (Vector Double) -> Double
{-# INLINE mIndexDouble #-}
mIndexDouble !i !j !v = v V.! i V.! j

mIndexCell :: Int -> Int -> Vector (Vector Cell) -> Cell
{-# INLINE mIndexCell #-}
mIndexCell !i !j !v = v V.! i V.! j




windGrid :: SubGrid Cell -> SubGrid Double
windGrid = V.imap (\i v -> V.imap (\j (Cell _ _ _ Information{..}) -> calculate i j uwind vwind) v)
    where
        calculate :: Int -> Int -> Double -> Double -> Double
        calculate 1 1 _ _ = 1
        calculate i j uwind vwind = if wind < 1 then 1 else wind
            where
                wind = windVectorDecomposition i j (windDirection uwind vwind) (windForceVector uwind vwind)




terrainGrid :: SubGrid Cell -> SubGrid Double
terrainGrid _ = V.fromList ([ V.fromList def, V.fromList def, V.fromList def])
    where
        def = [1, 1, 1]


findBigger :: Grid Cell -> (Int, Int, Double)
findBigger = foldAutomata (\i j acc (Cell s _ _ _) -> selectNext i j s acc) (0, 0, 0)
    where
        selectNext :: Int -> Int -> Double -> (Int, Int, Double) -> (Int, Int, Double)
        selectNext i j s (pi, pj, p)
            | s > p = (i, j, s)
            | otherwise = (pi, pj, p)



-- | TODO: La temperatura debería ser por celda, no global, pero lo usamos como
-- aproximación
runSimulation :: Double -> Information -> Int -> V.Vector (V.Vector Coordinate) -> (Int, Grid Cell)
runSimulation velocidad info iters coords = 
    let grid = runSimulation' iters (initialize info coords)
    in (findSpread grid, grid)
    where
        runSimulation' :: Int -> Grid Cell -> Grid Cell
        runSimulation' 0 !grid = grid
        runSimulation' n !grid =
            let poke = runRule rule grid
                (i, j, bigger) = findBigger poke
            in runSimulation' (n - 1) . mapAutomata (fixAutomata bigger) $ poke

        fixAutomata :: Double -> Cell -> Cell
        fixAutomata _ c@(Cell _ True _ _) = c
        fixAutomata d (Cell s False cs inf) = Cell state isBurned cs inf
            where
                state = s / d
                isBurned = if state >= 1 then True else False
       
        findSpread :: Grid Cell -> Int
        findSpread grid = maximum [derecha, izquierda, arriba, abajo]
            where
                derecha :: Int
                derecha = buscarj [center..(V.length grid - 1)]

                izquierda :: Int 
                izquierda = buscarj [0..center]

                arriba :: Int
                arriba = buscari [0..center]

                abajo :: Int
                abajo = buscari [center..(V.length grid - 1)]

                center :: Int
                center = V.length grid `div` 2

                buscarj :: [Int] -> Int
                buscarj = foldl' (\acc x -> if (state $ grid V.! center V.! x) == 1.0 then acc + 1 else acc) 0

                buscari :: [Int] -> Int
                buscari = foldl' (\acc x -> if (state $ grid V.! x V.! center) == 1.0 then acc + 1 else acc) 0

