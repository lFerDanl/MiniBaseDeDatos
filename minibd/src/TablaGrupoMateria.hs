module TablaGrupoMateria (listaGpMaterias, idGpMateria, sigGpMateria, codGpMateria, getGpMateria, setGpMateria, updateGpMateria, deleteGpMateria, deleteGpMateriaProf, deleteGpMateriaMat, validacionGpMateria) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Tipo de datos de la lista de gpMaterias
type GpMateria = [String]

-- Variable global que contiene la lista de gpMaterias
gpMateriasGlobal :: IORef [[String]]
gpMateriasGlobal = unsafePerformIO (newIORef [])

listaGpMaterias :: IO [[String]]
listaGpMaterias = readIORef gpMateriasGlobal

idGpMateria :: [String] -> String
idGpMateria [idGM, _, _] = idGM
idGpMateria [] = "-1"

sigGpMateria :: [String] -> String
sigGpMateria [ _, sigGM, _] = sigGM
sigGpMateria [] = "-1"

codGpMateria :: [String] -> String
codGpMateria [ _, _, codGM] = codGM
codGpMateria [] = "-1"

getGpMateria :: String -> IO [String]
getGpMateria idBuscado = do
    gpMaterias <- readIORef gpMateriasGlobal
    return (recorrerLista gpMaterias idBuscado)

setGpMateria :: String -> String -> IO ()
setGpMateria sigla codigo = do
    gpMaterias <- readIORef gpMateriasGlobal
    let nuevoId = show (read (getLastId gpMaterias) + 1)
    let nuevoGpMateria = [nuevoId, sigla, codigo] 
    writeIORef gpMateriasGlobal (gpMaterias ++ [nuevoGpMateria])
    putStrLn "Grupo-Materia agregado exitosamente."

updateGpMateria :: String -> String -> String -> IO ()
updateGpMateria idBuscado nuevaSigla nuevoCodigo = do
    gpMaterias <- readIORef gpMateriasGlobal
    let listaActualizada = actualizarGpMateria gpMaterias idBuscado nuevaSigla nuevoCodigo
    writeIORef gpMateriasGlobal listaActualizada
    putStrLn "Grupo-Materia actualizado exitosamente."

actualizarGpMateria :: [[String]] -> String -> String -> String -> [[String]]
actualizarGpMateria [] _ _ _ = []
actualizarGpMateria (x:l) idBuscado nuevaSigla nuevoCodigo = 
    if idBuscado == idGpMateria x
        then [idBuscado, nuevaSigla, nuevoCodigo] : l
        else x : actualizarGpMateria l idBuscado nuevaSigla nuevoCodigo

deleteGpMateria :: String -> IO()
deleteGpMateria idBuscado = do
    gpMaterias <- readIORef gpMateriasGlobal
    let listaActualizada = eliminarGpMateria gpMaterias idBuscado
    writeIORef gpMateriasGlobal listaActualizada
    putStrLn "Grupo-Materia eliminado exitosamente."

eliminarGpMateria :: [[String]] -> String -> [[String]]
eliminarGpMateria [] _ = []
eliminarGpMateria (x:l) idBuscado = 
    if idBuscado == idGpMateria x
        then l
        else x : eliminarGpMateria l idBuscado

deleteGpMateriaProf :: String -> IO()
deleteGpMateriaProf codProfesor = do
    gpMaterias <- readIORef gpMateriasGlobal
    let listaActualizada = eliminarGpMateriaProf gpMaterias codProfesor
    writeIORef gpMateriasGlobal listaActualizada

eliminarGpMateriaProf :: [[String]] -> String -> [[String]]
eliminarGpMateriaProf [] _ = []
eliminarGpMateriaProf (x:l) codProfesor = 
    if codProfesor == codGpMateria x
        then eliminarGpMateriaProf l codProfesor
        else x : eliminarGpMateriaProf l codProfesor

deleteGpMateriaMat :: String -> IO()
deleteGpMateriaMat sigMateria = do
    gpMaterias <- readIORef gpMateriasGlobal
    let listaActualizada = eliminarGpMateriaMat gpMaterias sigMateria
    writeIORef gpMateriasGlobal listaActualizada

eliminarGpMateriaMat :: [[String]] -> String -> [[String]]
eliminarGpMateriaMat [] _ = []
eliminarGpMateriaMat (x:l) sigMateria = 
    if sigMateria == sigGpMateria x
        then eliminarGpMateriaMat l sigMateria
        else x : eliminarGpMateriaMat l sigMateria

getLastId :: [[String]] -> String
getLastId [] = "0" 
getLastId x = idGpMateria (mylast x)

mylast :: [[String]] -> [String]
mylast [] = []
mylast [x] = x
mylast (_:l) = mylast l

recorrerLista :: [[String]] -> String -> [String]
recorrerLista [] _ = []  
recorrerLista (x:l) idBuscado =  if head x == idBuscado  
                                    then x      
                                    else recorrerLista l idBuscado

validacionGpMateria :: String -> IO Bool
validacionGpMateria idBuscado = do
    gpMaterias <- readIORef gpMateriasGlobal
    return (recorrerLista gpMaterias idBuscado /= [])