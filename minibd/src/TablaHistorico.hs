module TablaHistorico (listaHistoricos, regHistorico, idHistorico, notaHistorico, getHistoricosAlumno, setHistorico, updateHistorico, deleteHistorico, deleteHistoricoAlumno) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Tipo de datos de la lista de historicos
type Historico = [String]

-- Variable global que contiene la lista de historicos
historicosGlobal :: IORef [[String]]
historicosGlobal = unsafePerformIO (newIORef [])

listaHistoricos :: IO [[String]]
listaHistoricos = readIORef historicosGlobal

regHistorico :: [String] -> String
regHistorico [regH, _, _] = regH
regHistorico [] = "-1"

idHistorico :: [String] -> String
idHistorico [ _, idH, _] = idH
idHistorico [] = "-1"

notaHistorico :: [String] -> String
notaHistorico [ _, _, notaH] = notaH
notaHistorico [] = "-1"

getHistoricosAlumno :: String -> IO [[String]]
getHistoricosAlumno regBuscado = do
    historicos <- readIORef historicosGlobal
    return (recorrerLista historicos regBuscado)

setHistorico :: String -> String -> String -> IO ()
setHistorico registro id nota = do
    historicos <- readIORef historicosGlobal
    let nuevoHistorico = [registro, id, nota] 
    writeIORef historicosGlobal (historicos ++ [nuevoHistorico])
    putStrLn "Historico agregado exitosamente."

updateHistorico :: String -> String -> String -> IO ()
updateHistorico regBuscado idBuscado nuevaNota = do
    historicos <- readIORef historicosGlobal
    let listaActualizada = actualizarHistorico historicos regBuscado idBuscado nuevaNota
    writeIORef historicosGlobal listaActualizada
    putStrLn "Historico actualizado exitosamente."

actualizarHistorico :: [[String]] -> String -> String -> String -> [[String]]
actualizarHistorico [] _ _ _= []
actualizarHistorico (x:l) regBuscado idBuscado nuevaNota = 
    if (regBuscado == regHistorico x) && (idBuscado == idHistorico x)
        then [regBuscado, idBuscado, nuevaNota] : l
        else x : actualizarHistorico l regBuscado idBuscado nuevaNota

deleteHistorico :: String -> String -> IO()
deleteHistorico regBuscado idBuscado = do
    historicos <- readIORef historicosGlobal
    let listaActualizada = eliminarHistorico historicos regBuscado idBuscado
    writeIORef historicosGlobal listaActualizada
    putStrLn "Historico eliminado exitosamente."

eliminarHistorico :: [[String]] -> String -> String -> [[String]]
eliminarHistorico [] _ _ = []
eliminarHistorico (x:l) regBuscado idBuscado = 
    if (regBuscado == regHistorico x) && (idBuscado == idHistorico x)
        then l
        else x : eliminarHistorico l regBuscado idBuscado

deleteHistoricoAlumno :: String -> IO()
deleteHistoricoAlumno regBuscado = do
    historicos <- readIORef historicosGlobal
    let listaActualizada = eliminarHistoricoAlumno historicos regBuscado
    writeIORef historicosGlobal listaActualizada

eliminarHistoricoAlumno :: [[String]] -> String -> [[String]]
eliminarHistoricoAlumno [] _ = []
eliminarHistoricoAlumno (x:l) regBuscado = 
    if (regBuscado == regHistorico x)
        then eliminarHistoricoAlumno l regBuscado
        else x : eliminarHistoricoAlumno l regBuscado

recorrerLista :: [[String]] -> String -> [[String]]
recorrerLista [] _ = []  
recorrerLista (x:l) regBuscado =  if head x == regBuscado  
                                    then [x] ++ recorrerLista l regBuscado     
                                    else recorrerLista l regBuscado

