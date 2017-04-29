{-# LANGUAGE RecordWildCards #-}
module Database.Query where


import Database.Models
import Database.Esqueleto
import Database.Types
import Data.Maybe (listToMaybe)
import Control.Monad (forM)


obtenerIncendiosActivos :: Query [IncendioJSON]
obtenerIncendiosActivos = do
    incendios <- select $
                    from $ \i -> do
                    where_ (i ^. IncendioActivo ==. val True)
                    return i

    forM incendios $ \i -> do
        reportes <- obtenerReportes (entityKey i)
        coordenadas <- obtenerCoordenadas (entityKey i)
        satelite <- getMaybe (incendioSatelite . entityVal $ i)
        let incendio = i
        return IncendioJSON{..}


obtenerReportes :: Key Incendio -> Query [Entity Reporte]
obtenerReportes key = select $
    from $ \(ri, r) -> do
    where_ (ri ^. ReporteIncendioIncendio ==. val key)
    where_ (ri ^. ReporteIncendioReporte ==. r ^. ReporteId)
    return r

obtenerCoordenadas :: Key Incendio -> Query [Entity Coordenada]
obtenerCoordenadas key = select $
    from $ \c -> do
    where_ (c ^. CoordenadaIncendio ==. val key)
    return c
