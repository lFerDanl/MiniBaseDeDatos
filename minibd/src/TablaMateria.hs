module TablaMateria (listaMaterias, nombreMateria, sigMateria, getMateria, setMateria, updateMateria, deleteMateria, validacionMateria) where

import TablaGrupoMateria (deleteGpMateriaMat)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

type Materia = [String]

materiasGlobal :: IORef [[String]]
materiasGlobal = unsafePerformIO (newIORef [
    ["INF110", "Introduccion a la Programacion"],
    ["INF220", "Estructuras de Datos"],
    ["INF323", "Sistemas Operativos"],
    ["INF312", "Bases de Datos"],
    ["INF329", "Compiladores"]])

listaMaterias :: IO [[String]]
listaMaterias = readIORef materiasGlobal

nombreMateria :: [String] -> String
nombreMateria [_, nomb] = nomb 
nombreMateria [] = "No existe este Materia" 

sigMateria :: [String] -> String
sigMateria [sigM, _] = sigM
sigMateria [] = "-1"

getMateria :: String -> IO [String]
getMateria siglaBuscada = do
    materias <- readIORef materiasGlobal
    return (recorrerLista materias siglaBuscada)

setMateria :: String -> String -> IO ()
setMateria sigla nomb  
    | null sigla = putStrLn "No se ingreso la sigla."
    | null nomb = putStrLn "No se ingreso el nombre."
    | otherwise = do
    materias <- readIORef materiasGlobal
    if length sigla == 6  -- Aquí verificamos la longitud de la sigla
        then do
            let nuevaMateria = [sigla, nomb]
            writeIORef materiasGlobal (materias ++ [nuevaMateria])
            putStrLn "Materia agregada exitosamente."
        else do
            putStrLn "Error: La sigla debe tener exactamente 6 caracteres."

updateMateria :: String -> String -> String -> IO ()
updateMateria siglaBuscada nuevaSigla nuevoNombre 
    | null siglaBuscada = putStrLn "No se ingreso la sigla."
    | null nuevaSigla = putStrLn "No se ingreso el sigla."
    | null nuevoNombre = putStrLn "No se ingreso el nombre."
    | otherwise = do
    materias <- readIORef materiasGlobal
    if length siglaBuscada == 6  -- Aquí verificamos la longitud de la sigla
        then do
            let listaActualizada = actualizarMateria materias siglaBuscada nuevaSigla nuevoNombre
            writeIORef materiasGlobal listaActualizada
            putStrLn "Materia actualizada exitosamente."
        else do
            putStrLn "Error: La sigla debe tener exactamente 6 caracteres."


actualizarMateria :: [[String]] -> String -> String -> String -> [[String]]
actualizarMateria [] _ _ _ = []
actualizarMateria (x:l) siglaBuscada nuevaSigla nuevoNombre = 
    if siglaBuscada == sigMateria x
        then [nuevaSigla, nuevoNombre] : l
        else x : actualizarMateria l siglaBuscada nuevaSigla nuevoNombre

deleteMateria :: String -> IO()
deleteMateria siglaBuscada 
    | null siglaBuscada = putStrLn "No se ingreso la sigla."
    | otherwise = do
    materias <- readIORef materiasGlobal
    deleteGpMateriaMat siglaBuscada
    let listaActualizada = eliminarMateria materias siglaBuscada
    writeIORef materiasGlobal listaActualizada
    putStrLn "Materia eliminada exitosamente."

eliminarMateria :: [[String]] -> String -> [[String]]
eliminarMateria [] _ = []
eliminarMateria (x:l) siglaBuscada = 
    if siglaBuscada == sigMateria x
        then l
        else x : eliminarMateria l siglaBuscada

recorrerLista :: [[String]] -> String -> [String]
recorrerLista [] _ = []  
recorrerLista (x:l) siglaBuscada =  if head x == siglaBuscada  
                                    then x      
                                    else recorrerLista l siglaBuscada

validacionMateria :: String -> IO Bool
validacionMateria siglaBuscada = do
    materias <- readIORef materiasGlobal
    return (recorrerLista materias siglaBuscada /= [])