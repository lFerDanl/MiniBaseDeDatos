module TablaProfesor (listaProfesores, nombreProfesor, codProfesor, getProfesor, setProfesor, updateProfesor, deleteProfesor, validacionProfesor) where

import TablaGrupoMateria (deleteGpMateriaProf)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Tipo de datos de la lista de profesor
type Profesor = [String]

-- Variable global que contiene la lista de profesor
profesoresGlobal :: IORef [[String]]
profesoresGlobal = unsafePerformIO (newIORef [
    ["1001", "Ing. John Smith"],
    ["1002", "Ing. Emily Davis"],
    ["1003", "Ing. Michael Brown"],
    ["1004", "Ing. Sarah Wilson"],
    ["1005", "Ing. James Taylor"]])

listaProfesores :: IO [[String]]
listaProfesores = readIORef profesoresGlobal

nombreProfesor :: [String] -> String
nombreProfesor [_, nomb] = nomb 
nombreProfesor [] = "No existe este Profesor" 

codProfesor :: [String] -> String
codProfesor [codP, _] = codP
codProfesor [] = "-1"

getProfesor :: String -> IO [String]
getProfesor codBuscado = do
    profesores <- readIORef profesoresGlobal
    return (recorrerLista profesores codBuscado)

setProfesor :: String -> IO ()
setProfesor nomb
    | null nomb = putStrLn "No se ingreso el nombre."
    | otherwise = do
    profesores <- readIORef profesoresGlobal
    let nuevoCod = show (read (getLastCod profesores) + 1)
    let nuevoProfesor = [nuevoCod, nomb] 
    writeIORef profesoresGlobal (profesores ++ [nuevoProfesor])
    putStrLn "Profesor agregado exitosamente."

updateProfesor :: String -> String -> IO ()
updateProfesor codBuscado nuevoNombre 
    | null codBuscado = putStrLn "No se ingreso el código."
    | null nuevoNombre = putStrLn "No se ingreso el nombre."
    | otherwise = do
    profesores <- readIORef profesoresGlobal
    let listaActualizada = actualizarProfesor profesores codBuscado nuevoNombre
    writeIORef profesoresGlobal listaActualizada
    putStrLn "Profesor actualizado exitosamente."

actualizarProfesor :: [[String]] -> String -> String -> [[String]]
actualizarProfesor [] _ _ = []
actualizarProfesor (x:l) codBuscado nuevoNombre = 
    if codBuscado == codProfesor x
        then [codBuscado, nuevoNombre] : l
        else x : actualizarProfesor l codBuscado nuevoNombre

deleteProfesor :: String -> IO()
deleteProfesor codBuscado 
    | null codBuscado = putStrLn "No se ingreso el codigo."
    | otherwise = do
    profesores <- readIORef profesoresGlobal
    deleteGpMateriaProf codBuscado
    let listaActualizada = eliminarProfesor profesores codBuscado
    writeIORef profesoresGlobal listaActualizada
    putStrLn "Profesor eliminado exitosamente."

eliminarProfesor :: [[String]] -> String -> [[String]]
eliminarProfesor [] _ = []
eliminarProfesor (x:l) codBuscado = 
    if codBuscado == codProfesor x
        then l
        else x : eliminarProfesor l codBuscado
   

getLastCod :: [[String]] -> String
getLastCod [] = "0" 
getLastCod x = codProfesor (mylast x)

mylast :: [[String]] -> [String]
mylast [] = []
mylast [x] = x
mylast (_:l) = mylast l

recorrerLista :: [[String]] -> String -> [String]
recorrerLista [] _ = []  
recorrerLista (x:l) codBuscado =  if head x == codBuscado  
                                    then x      
                                    else recorrerLista l codBuscado

validacionProfesor :: String -> IO Bool
validacionProfesor codBuscado = do
    profesores <- readIORef profesoresGlobal
    return (recorrerLista profesores codBuscado /= [])