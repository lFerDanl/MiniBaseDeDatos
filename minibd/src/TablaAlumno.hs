module TablaAlumno (listaAlumnos, nombreAlumno, regAlumno, getAlumno, setAlumno, updateAlumno, deleteAlumno, validacionAlumno) where

import TablaHistorico (deleteHistoricoAlumno)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Tipo de datos de la lista de alumnos
type Alumno = [String]

-- Variable global que contiene la lista de alumnos
alumnosGlobal :: IORef [[String]]
alumnosGlobal = unsafePerformIO (newIORef [
    ["20101", "Alice Johnson"],
    ["20102", "Bob Smith"],
    ["20103", "Charlie Brown"],
    ["20104", "Diana Prince"],
    ["20105", "Ethan Hunt"],
    ["20106", "Fiona Clark"],
    ["20107", "George Lopez"],
    ["20108", "Hannah Lee"],
    ["20109", "Isaac Newton"],
    ["20110", "Julia Roberts"]])

listaAlumnos :: IO [[String]]
listaAlumnos = readIORef alumnosGlobal

nombreAlumno :: [String] -> String
nombreAlumno [_, nomb] = nomb 
nombreAlumno [] = "No existe este Alumno" 

regAlumno :: [String] -> String
regAlumno [regA, _] = regA
regAlumno [] = "-1"

getAlumno :: String -> IO [String]
getAlumno regBuscado = do
    alumnos <- readIORef alumnosGlobal
    return (recorrerLista alumnos regBuscado)

setAlumno :: String -> IO ()
setAlumno nomb 
    | null nomb = putStrLn "No se ingreso el nombre."
    | otherwise = do
    alumnos <- readIORef alumnosGlobal
    let nuevoReg = show (read (getLastReg alumnos) + 1)
    let nuevoAlumno = [nuevoReg, nomb] 
    writeIORef alumnosGlobal (alumnos ++ [nuevoAlumno])
    putStrLn "Alumno agregado exitosamente."

updateAlumno :: String -> String -> IO ()
updateAlumno regBuscado nuevoNombre 
    | null regBuscado = putStrLn "No se ingreso el registro."
    | null nuevoNombre = putStrLn "No se ingreso el nombre."
    | otherwise = do
    alumnos <- readIORef alumnosGlobal
    let listaActualizada = actualizarAlumno alumnos regBuscado nuevoNombre
    writeIORef alumnosGlobal listaActualizada
    putStrLn "Alumno actualizado exitosamente."

actualizarAlumno :: [[String]] -> String -> String -> [[String]]
actualizarAlumno [] _ _ = []
actualizarAlumno (x:l) regBuscado nuevoNombre = 
    if regBuscado == regAlumno x
        then [regBuscado, nuevoNombre] : l
        else x : actualizarAlumno l regBuscado nuevoNombre

deleteAlumno :: String -> IO()
deleteAlumno regBuscado 
    | null regBuscado = putStrLn "No se ingreso el registro."
    | otherwise = do
    alumnos <- readIORef alumnosGlobal
    deleteHistoricoAlumno regBuscado
    let listaActualizada = eliminarAlumno alumnos regBuscado
    writeIORef alumnosGlobal listaActualizada
    putStrLn "Alumno eliminado exitosamente."

eliminarAlumno :: [[String]] -> String -> [[String]]
eliminarAlumno [] _ = []
eliminarAlumno (x:l) regBuscado = 
    if regBuscado == regAlumno x
        then l
        else x : eliminarAlumno l regBuscado
   

getLastReg :: [[String]] -> String
getLastReg [] = "0" 
getLastReg x = regAlumno (mylast x)

mylast :: [[String]] -> [String]
mylast [] = []
mylast [x] = x
mylast (_:l) = mylast l

recorrerLista :: [[String]] -> String -> [String]
recorrerLista [] _ = []  
recorrerLista (x:l) regBuscado =  if head x == regBuscado  
                                    then x      
                                    else recorrerLista l regBuscado

validacionAlumno :: String -> IO Bool
validacionAlumno regBuscado = do
    alumnos <- readIORef alumnosGlobal
    return (recorrerLista alumnos regBuscado /= [])
                        

