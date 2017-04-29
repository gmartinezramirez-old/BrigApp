{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Automata where


import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Aeson
import qualified Data.HashMap.Lazy as HM
import Data.Scientific
import Data.Convertible
import qualified Data.ByteString.Lazy as BS


type Grid a = Vector (Vector a)
type SubGrid a = Vector (Vector a)

initial :: Int -> Int -> (Int -> Int -> a) -> Grid a
initial n m f = V.generate n (\i -> V.generate m (f i))

runRule :: (SubGrid a -> a -> a) -> Grid a -> Grid a
runRule f grid = V.imap (\i v -> V.imap (\j e -> if isValid i j then f (neighborhoods i j grid) e else e) v) grid
    where
        -- | Revisamos si estamos en los bordes del autómata, no queremos
        -- lidiar con casos especiales asi que los bordes no los ejecutamos no
        -- más
        isValid i j
            | i == 0 || j == 0               = False
            | i + 1 == iMax || j + 1 == jMax = False
            | otherwise                      = True

        iMax = V.length grid
        jMax = if iMax > 0 then V.length (grid V.! 0) else 0

-- | TODO El map puede ser muy lento, hay que ver como se podría optimizar. Por
-- lo menos el slice es rápido dado que es O(1). Igual debería ser O(1) el map
-- dado que son siempre 3 elementos en el vector que se le pasa, pero no estoy
-- seguro si eso implica una copia del vector de entrada hacia el vector de
-- salida con los nuevos elementos. Debería dado que estamos usando un vector
-- inmutable. Hay que ver una forma más rápida de hacerlo.
neighborhoods :: Int -> Int -> Grid a -> SubGrid a
neighborhoods !i !j = V.map (V.slice (j - 1) 3) . V.slice (i - 1) 3


run :: (SubGrid a -> a -> a) -> Grid a -> [Grid a]
run rule grid = iterate (runRule rule) grid


runUntil :: Int -> (SubGrid a -> a -> a) -> Grid a -> Grid a
runUntil 0 _ grid = grid
runUntil max rule grid = runUntil (max - 1) rule (runRule rule grid)


createJSON :: (ToJSON a) => [Grid a] -> Value
createJSON = toJSON . map create
    where
        create :: (ToJSON a) => Grid a -> Array
        create = V.foldl' (\acc v -> acc V.++ v) V.empty . V.imap (\i v -> V.imap (\j e -> add i j (toJSON e)) v)

        add :: Int -> Int -> Value -> Value
        add i j val =
            case val of
                Object obj -> Object $ HM.insert "y" (Number (fromInteger (convert i))) (HM.insert "x" (Number (fromInteger (convert j))) obj)
                _ -> error "Can't encode the json"


writeJSON :: ToJSON a => FilePath -> [Grid a] -> IO ()
writeJSON file xs =
    let json = encode $ createJSON xs
    in BS.writeFile file json

foldAutomata :: (Int -> Int -> b -> a -> b) -> b -> Grid a -> b
foldAutomata f init grid = V.ifoldl' (\acc i v -> V.ifoldl' (\acc2 j e -> f i j acc2 e) acc v) init grid

mapAutomata :: (a -> a) -> Grid a -> Grid a
mapAutomata f = V.map (\v -> V.map f v)


pprint :: Show a => Grid a -> IO ()
pprint grid = do
    V.forM_ grid $ \v -> do
        V.forM_ v $ \x -> do
            putStr $ " [" ++ show x ++ "] "
        putStrLn ""
