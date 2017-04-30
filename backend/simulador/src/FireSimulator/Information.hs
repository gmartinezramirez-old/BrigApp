module FireSimulator.Information where

import FireSimulator.Types

type UWind = Double
type VWind = Double
type WindForce = Double




toRandians :: Double -> Double
toRandians n = n*pi/180.0

toAngle :: Double -> Double
toAngle n = n/pi*180.0

windAngle :: Double
windAngle = sin $ toRandians 45.0

getUV :: Double -> Double -> (UWind, VWind)
getUV vector angle = (vector * (cos (toRandians angle)), vector * (sin (toRandians angle)))


windVectorDecomposition :: Int -> Int -> WindDirection -> WindForce -> Double
windVectorDecomposition 1 0 WindN _ = 1
windVectorDecomposition 1 0 WindS _ = 1
windVectorDecomposition 1 0 WindW windForce = windForce
windVectorDecomposition 1 0 WindE _ = 1
windVectorDecomposition 1 0 WindNW windForce = windForce*windAngle
windVectorDecomposition 1 0 WindNE _ = 1
windVectorDecomposition 1 0 WindSW windForce = windForce*windAngle
windVectorDecomposition 1 0 WindSE _ = 1
windVectorDecomposition 2 0 WindN _ = 1
windVectorDecomposition 2 0 WindS windForce = windForce*windAngle
windVectorDecomposition 2 0 WindW windForce = windForce*windAngle
windVectorDecomposition 2 0 WindE _ = 1
windVectorDecomposition 2 0 WindNW _ = 1
windVectorDecomposition 2 0 WindNE _ = 1
windVectorDecomposition 2 0 WindSW windForce = windForce
windVectorDecomposition 2 0 WindSE _ = 1
windVectorDecomposition 2 1 WindN _ = 1
windVectorDecomposition 2 1 WindS windForce = windForce
windVectorDecomposition 2 1 WindW _ = 1
windVectorDecomposition 2 1 WindE _ = 1
windVectorDecomposition 2 1 WindNW _ = 1
windVectorDecomposition 2 1 WindNE _ = 1
windVectorDecomposition 2 1 WindSW windForce = windForce*windAngle
windVectorDecomposition 2 1 WindSE windForce = windForce*windAngle
windVectorDecomposition 2 2 WindN _ = 1
windVectorDecomposition 2 2 WindS windForce = windForce*windAngle
windVectorDecomposition 2 2 WindW _ = 1
windVectorDecomposition 2 2 WindE windForce = windForce*windAngle
windVectorDecomposition 2 2 WindNW _ = 1
windVectorDecomposition 2 2 WindNE _ = 1
windVectorDecomposition 2 2 WindSW _ = 1
windVectorDecomposition 2 2 WindSE windForce = windForce
windVectorDecomposition 1 2 WindN _ = 1
windVectorDecomposition 1 2 WindS _ = 1
windVectorDecomposition 1 2 WindW _ = 1
windVectorDecomposition 1 2 WindE windForce = windForce
windVectorDecomposition 1 2 WindNW _ = 1
windVectorDecomposition 1 2 WindNE windForce = windForce*windAngle
windVectorDecomposition 1 2 WindSW _ = 1
windVectorDecomposition 1 2 WindSE windForce = windForce*windAngle
windVectorDecomposition 0 2 WindN windForce = windForce*windAngle
windVectorDecomposition 0 2 WindS _ = 1
windVectorDecomposition 0 2 WindW _ = 1
windVectorDecomposition 0 2 WindE windForce = windForce*windAngle
windVectorDecomposition 0 2 WindNW _ = 1
windVectorDecomposition 0 2 WindNE windForce = windForce
windVectorDecomposition 0 2 WindSW _ = 1
windVectorDecomposition 0 2 WindSE _ = 1
windVectorDecomposition 0 1 WindN windForce = windForce
windVectorDecomposition 0 1 WindS _ = 1
windVectorDecomposition 0 1 WindW _ = 1
windVectorDecomposition 0 1 WindE _ = 1
windVectorDecomposition 0 1 WindNW windForce = windForce*windAngle
windVectorDecomposition 0 1 WindNE windForce = windForce*windAngle
windVectorDecomposition 0 1 WindSW _ = 1
windVectorDecomposition 0 1 WindSE _ = 1
windVectorDecomposition 0 0 WindN windForce = windForce*windAngle
windVectorDecomposition 0 0 WindS _ = 1
windVectorDecomposition 0 0 WindW windForce = windForce*windAngle
windVectorDecomposition 0 0 WindE _ = 1
windVectorDecomposition 0 0 WindNW windForce = windForce
windVectorDecomposition 0 0 WindNE _ = 1
windVectorDecomposition 0 0 WindSW _ = 1
windVectorDecomposition 0 0 WindSE _ = 1
windVectorDecomposition _ _ _ _ = error "windVectorDecomposition caso no valido"



windForceVector :: UWind -> VWind -> Double
windForceVector uwind vwind = velocity
    where
        velocity = sqrt (uwind^2 + vwind^2)

        -- Escalamos la velocidad del viento entre 1 y 2 asumiendo un máximo de
        -- 8 m/s
        force  = 2.0 / 8.0 * velocity


windDirection :: UWind -> VWind -> WindDirection
windDirection uwind vwind
    | angle > 0 && angle < 90 = WindSW
    | angle > 90 && angle < 180 = WindSE
    | angle < 0 && angle > (-90) = WindNW
    | angle < (-90) && angle > (-180) = WindNE
    | angle == 0 = WindW
    | angle == 180 = WindE
    | angle == 90 = WindS
    | angle == (-90) = WindN
    | otherwise = error (show angle)

    where
        angle = toAngle (atan2 vwind uwind)
