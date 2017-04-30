{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
module Database.Models where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Incendio json
        activo Bool
        confirmado Bool
        satelite SateliteId Maybe
        deriving Show

    Satelite json
        lat Double
        lng Double
        nombre Text
        brightness Double
        date UTCTime
        confidence Text
        deriving Show

    ReporteIncendio json
        incendio IncendioId
        reporte ReporteId
        deriving Show

    Reporte json
        razon Text
        lat Double
        lng Double
        date UTCTime
        deriving Show

    -- Coordenadas conocidas del incendio
    Coordenada json
        incendio IncendioId
        lat Double
        lng Double
        deriving Show

    Simulacion json
        incendio IncendioId
        lat Double
        lng Double
        estado Double

    Elevacion
        lat Double
        lng Double
        val Double

|]

type Query a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a


runDB :: Query a -> IO a
runDB query = runSqlite "database.sqlite3" query

migrate :: IO ()
migrate = runDB $ runMigration migrateAll


getMaybe :: (PersistStoreWrite SqlBackend, PersistRecordBackend a SqlBackend) =>  Maybe (Key a) -> Query (Maybe (Entity a))
getMaybe Nothing = return Nothing
getMaybe (Just k) = getEntity k
