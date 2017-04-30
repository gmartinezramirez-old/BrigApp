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
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Data.Angle


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
            in runSimulation' (n - 1) . change n . mapAutomata (fixAutomata bigger) $ poke
            {-in runSimulation' (n - 1) . mapAutomata (fixAutomata bigger) $ poke-}

        fixAutomata :: Double -> Cell -> Cell
        fixAutomata _ c@(Cell _ True _ _) = c
        fixAutomata d (Cell s False cs inf) = Cell state isBurned cs inf
            where
                state = s / d
                isBurned = if state >= 1 then True else False

        change :: Int -> Grid Cell -> Grid Cell
        change 100 grid = V.map (\v -> V.map (\x -> x { information = Information (8) (-8)}) v) grid
        change _ grid = grid
       
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

data Agregando = Agregando
    { uno :: Maybe (Double, Double)
    , dos :: Maybe (Double, Double)
    } deriving Show

poligono :: Grid Cell -> [(Double, Double)]
poligono grid = 
    let filtered = V.foldl' (\acc v -> (V.foldl' (\acc' x -> agregar acc' x ) (Agregando Nothing Nothing) v) : acc) [] grid
        flat = concat . map fn $ filtered
    in (head flat) : sortP (head flat) (tail flat)

    where
        agregar :: Agregando -> Cell -> Agregando
        agregar a@(Agregando Nothing Nothing) c
            | state c >= 1.0 = Agregando (Just (lat . position $ c, lng . position $ c)) Nothing
            | otherwise = a
        agregar a@(Agregando (Just x) _) c
            | state c >= 1.0 = Agregando (Just x) (Just (lat . position $ c, lng . position $ c))
            | otherwise = a
        agregar x _ = error $ show x

        sortP :: (Double, Double) -> [(Double, Double)] -> [(Double, Double)]
        sortP x [] = [x]
        sortP now all@(x:xs) =
            let m = minimun' x xs
            in m : sortP m (delete m all)
           where 
            minimun' ::  (Double, Double) -> [(Double, Double)] -> (Double, Double)
            minimun' current [] = current
            minimun' current (next:xs) = if haversineDouble current now < haversineDouble next now then minimun' current xs else minimun' next xs

        fn :: Agregando -> [(Double, Double)]
        fn (Agregando (Just t) (Just t2)) = [t, t2]
        fn (Agregando (Just t) Nothing) = [t]
        fn (Agregando Nothing Nothing) = []


writePoligonoJSON :: FilePath -> [(Double, Double)] -> IO ()
writePoligonoJSON file pol = do
    let json = encode pol
    BS.writeFile file json



haversine :: (Degrees Double, Degrees Double) -> (Degrees Double, Degrees Double) -> Double
haversine coor1 coor2 =  (\x -> x * 2 * 6367 * 1000) . asin . sqrt $ haversine' (tupleMap radians coor1) (tupleMap radians coor2)
    where
        tupleMap :: (a -> b) -> (a, a) -> (b, b)
        tupleMap f (x, y) = (f x, f y)

        haversine' :: (Radians Double, Radians Double) -> (Radians Double, Radians Double) -> Double
        haversine' (Radians lat1, Radians lng1) (Radians lat2, Radians lng2) =
            sin ((lat2 - lat1) / 2) ^ 2 + cos lat1 * cos lat2 * sin ((lng2 - lng1) / 2) ^ 2

haversineDouble :: (Double, Double) -> (Double, Double) -> Double
haversineDouble (x, y) (x', y') = haversine (Degrees x, Degrees y) (Degrees x', Degrees y')
