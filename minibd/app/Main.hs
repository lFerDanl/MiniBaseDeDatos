module Main (main) where

import TablaAlumno (listaAlumnos, nombreAlumno, regAlumno, getAlumno, setAlumno, updateAlumno, deleteAlumno, validacionAlumno) 
import TablaProfesor (listaProfesores, nombreProfesor, codProfesor, getProfesor, setProfesor, updateProfesor, deleteProfesor, validacionProfesor)
import TablaMateria (listaMaterias, nombreMateria, sigMateria, getMateria, setMateria, updateMateria, deleteMateria, validacionMateria)
import TablaGrupoMateria (listaGpMaterias, idGpMateria, getGpMateria, setGpMateria, updateGpMateria, deleteGpMateria, validacionGpMateria)
import TablaHistorico (listaHistoricos, regHistorico, getHistoricosAlumno, setHistorico, updateHistorico, deleteHistorico)

import System.Process (callCommand)
import Text.Printf (printf)

-- main :: IO ()
-- main = do

-- Funcion principal
main :: IO ()
main = menuPrincipal

-- Función para limpiar la pantalla (funciona en sistemas Unix-like y Windows)
-- clearScreen :: IO ()
-- clearScreen = putStr "\ESC[2J\ESC[H"
clearScreen :: IO ()
clearScreen = do
  callCommand "cls"  -- En Windows usa 'cls'
  -- callCommand "clear"  -- Para sistemas Unix-like


-- Menu principal
menuPrincipal :: IO ()
menuPrincipal = do
  clearScreen
  putStrLn "\nMenú Principal"
  putStrLn "1. Altas"
  putStrLn "2. Bajas"
  putStrLn "3. Mostrar Tablas"
  putStrLn "4. Salir"
  putStrLn " "
  putStrLn "Seleccione una opción: "
  opcion <- getLine
  putStrLn " "
  clearScreen
  case opcion of
    "1" -> menuAltas
    "2" -> menuBajas
    "3" -> menuTablas
    "4" -> putStrLn "Saliendo del programa..."
    _    -> do
      putStrLn "Opción no válida, intente nuevamente."
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuPrincipal

-- Menu de altas
menuAltas :: IO ()
menuAltas = do
  clearScreen
  putStrLn "\nMenú de Altas"
  putStrLn "1. Alta a Grupo Materia"
  putStrLn "2. Alta al Historico"
  putStrLn "3. Agregar Alumno"
  putStrLn "4. Agregar Profesor"
  putStrLn "5. Agregar Materia"
  putStrLn "6. Actualizar Alumno"
  putStrLn "7. Actualizar Profesor"
  putStrLn "8. Actualizar Materia"
  putStrLn "9. Volver"
  putStrLn " "
  putStrLn "Seleccione una opción: "
  opcion <- getLine
  putStrLn " "
  clearScreen
  case opcion of
    "1" -> do
      putStrLn "Grupo - Materia"
      putStrLn " "
      lista <- listaMaterias
      putStrLn "Lista de materias:"
      print lista
      putStrLn " "
      lista <- listaProfesores
      putStrLn "Lista de profesores:"
      print lista
      putStrLn " "
      putStrLn "Ingresa la Sigla de la Materia: "
      xsiglaMateria<-getLine
      putStrLn "Ingresa el Codigo del Profesor: "
      xcodigoProfesor<-getLine
      putStrLn " "
      validacionMateria xsiglaMateria >>= \esValidaMateria ->
        if esValidaMateria
          then do
            validacionProfesor xcodigoProfesor >>= \esValidoProfesor ->
              if esValidoProfesor
                then setGpMateria xsiglaMateria xcodigoProfesor
                else putStrLn "El codigo no es valido. No existe en la Base de Datos"
          else putStrLn "La sigla no es valida. No existe en la Base de Datos"
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuAltas
    "2" -> do
      putStrLn "Historico"
      putStrLn " "
      lista <- listaAlumnos
      putStrLn "Lista de alumnos:"
      print lista
      putStrLn " "
      lista <- listaGpMaterias
      putStrLn "Lista de Grupo Materias:"
      print lista
      putStrLn " "
      putStrLn "Ingresa el Registro del Alumno: "
      xregistroAlumno<-getLine
      putStrLn "Ingresa el Id del Grupo-Materia: "
      xidGrupoMateria<-getLine
      putStrLn "Ingresa la Nota del Alumno: "
      xNota<-getLine
      putStrLn " "
      case reads xNota :: [(Int, String)] of
        [(nota, "")] -> -- Conversión exitosa
          if nota >= 0 && nota <= 100
            then do
              validacionAlumno xregistroAlumno >>= \esValidoAlumno ->
                if esValidoAlumno
                  then do
                    validacionGpMateria xidGrupoMateria >>= \esValidoGpMateria ->
                      if esValidoGpMateria
                        then setHistorico xregistroAlumno xidGrupoMateria xNota
                        else putStrLn "El id no es válido. No existe en la Base de Datos."
                  else putStrLn "El registro no es válido. No existe en la Base de Datos."
            else putStrLn "La nota debe estar dentro del rango 0-100."
        _ -> putStrLn "La nota ingresada no es válida. Debe ser un número." 
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuAltas
    "3" -> do
      putStrLn "Alumno"
      putStrLn " "
      putStrLn "Ingresa el Nombre del Alumno: "
      xnombreAlumno<-getLine
      putStrLn " "
      setAlumno xnombreAlumno
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuAltas
    "4" -> do
      putStrLn "Profesor"
      putStrLn " "
      putStrLn "Ingresa el Nombre del Profesor: "
      xnombreProfesor<-getLine
      putStrLn " "
      setProfesor xnombreProfesor
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuAltas
    "5" -> do
      putStrLn "Materia"
      putStrLn " "
      putStrLn "Ingresa la Sigla de la Materia: "
      xsiglaMateria<-getLine
      putStrLn "Ingresa el Nombre de la Materia: "
      xnombreMateria<-getLine
      putStrLn " "
      setMateria xsiglaMateria xnombreMateria
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuAltas
    "6" -> do
      putStrLn "Alumno"
      putStrLn " "
      lista <- listaAlumnos
      putStrLn "Lista de alumnos:"
      print lista
      putStrLn " "
      putStrLn "Ingresa el Registro del Alumno: "
      xregistroAlumno<-getLine
      putStrLn "Ingresa el Nombre del Alumno: "
      xnombreAlumno<-getLine
      putStrLn " "
      validacionAlumno xregistroAlumno >>= \esValidoAlumno ->
        if esValidoAlumno
          then updateAlumno xregistroAlumno xnombreAlumno
          else putStrLn "El registro no es valido. No existe en la Base de Datos"
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuAltas
    "7" -> do
      putStrLn "Profesor"
      putStrLn " "
      lista <- listaProfesores
      putStrLn "Lista de profesores:"
      print lista
      putStrLn " "
      putStrLn "Ingresa el Codigo del Profesor: "
      xcodigoProfesor<-getLine
      putStrLn "Ingresa el Nombre del Profesor: "
      xnombreProfesor<-getLine
      putStrLn " "
      validacionProfesor xcodigoProfesor >>= \esValidoProfesor ->
        if esValidoProfesor
          then updateProfesor xcodigoProfesor xnombreProfesor
          else putStrLn "El codigo no es valido. No existe en la Base de Datos"
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuAltas
    "8" -> do
      putStrLn "Materia"
      putStrLn " "
      lista <- listaMaterias
      putStrLn "Lista de materias:"
      print lista
      putStrLn " "
      putStrLn "Ingresa la Sigla de la Materia: "
      xsiglaMateria<-getLine
      putStrLn "Ingresa la Sigla Nueva de la Materia: "
      xsiglaMateriaNueva<-getLine
      putStrLn "Ingresa el Nombre de la Materia: "
      xnombreMateria<-getLine
      putStrLn " "
      validacionMateria xsiglaMateria >>= \esValidoMateria ->
        if esValidoMateria
          then updateMateria xsiglaMateria xsiglaMateriaNueva xnombreMateria
          else putStrLn "La sigla no es valida. No existe en la Base de Datos"
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuAltas
    "9" -> menuPrincipal
    _    -> do
      putStrLn "Opción no válida, intente nuevamente."
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuAltas

-- Menu de bajas
menuBajas :: IO ()
menuBajas = do
  clearScreen
  putStrLn "\nMenú de Bajas"
  putStrLn "1. Eliminar de la Tabla Alumno"
  putStrLn "2. Eliminar de la Tabla Profesor"
  putStrLn "3. Eliminar de la Tabla Materia"
  putStrLn "4. Eliminar de la Tabla Grupo-Materia"
  putStrLn "5. Eliminar de la Tabla Historico"
  putStrLn "6. Volver"
  putStrLn " "
  putStrLn "Seleccione una opción: "
  opcion <- getLine
  clearScreen
  case opcion of
    "1" -> do
      putStrLn "Alumno"
      putStrLn " "
      lista <- listaAlumnos
      putStrLn "Lista de alumnos:"
      print lista
      putStrLn " "
      putStrLn "Ingrese el Registro del Alumno"
      xregistroAlumno <- getLine
      putStrLn " "
      validacionAlumno xregistroAlumno >>= \esValidoAlumno ->
        if esValidoAlumno
          then deleteAlumno xregistroAlumno
          else putStrLn "El registro no es valido. No existe en la Base de Datos"
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuBajas
    "2" -> do
      putStrLn "Profesor"
      putStrLn " "
      lista <- listaProfesores
      putStrLn "Lista de profesores:"
      print lista
      putStrLn " "
      putStrLn "Ingrese el Codigo del Profesor"
      xcodigoProfesor <- getLine
      putStrLn " "
      validacionProfesor xcodigoProfesor >>= \esValidoProfesor ->
        if esValidoProfesor
          then deleteProfesor xcodigoProfesor
          else putStrLn "El codigo no es valido. No existe en la Base de Datos"
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuBajas
    "3" -> do
      putStrLn "Materia"
      putStrLn " "
      lista <- listaMaterias
      putStrLn "Lista de materias:"
      print lista
      putStrLn " "
      putStrLn "Ingrese la Sigla de la Materia"
      xsiglaMateria <- getLine
      putStrLn " "
      validacionMateria xsiglaMateria >>= \esValidoMateria ->
        if esValidoMateria
          then deleteMateria xsiglaMateria
          else putStrLn "La sigla no es valida. No existe en la Base de Datos"
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuBajas
    "4" -> do
      putStrLn "Grupo-Materia"
      putStrLn " "
      lista <- listaGpMaterias
      putStrLn "Lista de Grupo-Materia:"
      print lista
      putStrLn " "
      putStrLn "Ingrese el ID del Grupo-Materia"
      xidGrupoMateria <- getLine
      putStrLn " "
      validacionGpMateria xidGrupoMateria >>= \esValidoGpMateria ->
        if esValidoGpMateria
          then deleteGpMateria xidGrupoMateria
          else putStrLn "El Id no es valido. No existe en la Base de Datos"
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuBajas
    "5" -> do
      putStrLn "Historico"
      putStrLn " "
      lista <- listaHistoricos
      putStrLn "Lista de Historicos:"
      print lista
      putStrLn " "
      putStrLn "Ingrese el Registro del Alumno: "
      xregistroAlumno <- getLine
      putStrLn "Ingrese el Id del Grupo-Materia: "
      xidGrupoMateria <- getLine
      putStrLn " "
      validacionAlumno xregistroAlumno >>= \esValidoAlumno ->
        if esValidoAlumno
          then do
            validacionGpMateria xidGrupoMateria >>= \esValidoGpMateria ->
              if esValidoGpMateria
                then deleteHistorico xregistroAlumno xidGrupoMateria
                else putStrLn "El id no es valido. No existe en la Base de Datos"
          else putStrLn "El registro no es valido. No existe en la Base de Datos"
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuBajas
    "6" -> menuPrincipal
    _    -> do
      putStrLn "Opción no válida, intente nuevamente."
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuBajas

-- Menu de bajas
menuTablas :: IO ()
menuTablas = do
  clearScreen
  putStrLn "\nMenú de Tablas"
  putStrLn "1. Mostrar la Tabla Alumno"
  putStrLn "2. Mostrar la Tabla Profesor"
  putStrLn "3. Mostrar la Tabla Materia"
  putStrLn "4. Mostrar la Tabla Grupo-Materia"
  putStrLn "5. Mostrar la Tabla Historico"
  putStrLn "6. Volver"
  putStrLn " "
  putStrLn "Seleccione una opción: "
  opcion <- getLine
  putStrLn " "
  clearScreen
  case opcion of
    "1" -> do
      lista <- listaAlumnos
      putStrLn "Alumnos"
      putStrLn " "
      putStrLn "Registro | Nombre"
      putStrLn "--------------------------"
      mapM_ (\[registro, nombre] -> printf "%-8s | %-15s\n" registro nombre) lista
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuTablas
    "2" -> do
      lista <- listaProfesores
      putStrLn "Profesores"
      putStrLn " "
      putStrLn "Codigo | Nombre"
      putStrLn "--------------------------"
      mapM_ (\[codigo, nombre] -> printf "%-6s | %-15s\n" codigo nombre) lista
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuTablas
    "3" -> do
      lista <- listaMaterias
      putStrLn "Materias"
      putStrLn " "
      putStrLn "Sigla  | Nombre"
      putStrLn "----------------------------------"
      mapM_ (\[sigla, nombre] -> printf "%-5s | %-15s\n" sigla nombre) lista
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuTablas
    "4" -> do
      lista <- listaGpMaterias
      putStrLn "Grupos - Materia"
      putStrLn " "
      putStrLn "ID  | Sigla  | Codigo"
      putStrLn "----------------------------------"
      mapM_ (\[id, sigla, codigo] -> printf "%-3s | %-6s | %-6s\n" id sigla codigo) lista
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuTablas
    "5" -> do
      lista <- listaHistoricos
      putStrLn "Historicos"
      putStrLn " "
      putStrLn "Registro | ID  | Nota"
      putStrLn "--------------------------"
      mapM_ (\[registro, id, nota] -> printf "%-8s | %-3s | %-6s\n" registro id nota) lista
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuTablas
    "6" -> menuPrincipal
    _    -> do
      putStrLn "Opción no válida, intente nuevamente."
      putStrLn " "
      putStrLn "Presione Enter para continuar"
      opcion<-getLine
      menuTablas



--   -- Agregar un historico y mostrar un mensaje de confirmación
--   setHistorico "1" "20" "51"
--   putStrLn "Historico agregado"

--   -- Imprimir la lista de historicos actualizada
--   lista <- listaHistoricos
--   putStrLn "Lista de historicos:"
--   print lista

--     -- Agregar un historico y mostrar un mensaje de confirmación
--   setHistorico "2" "30" "45"
--   putStrLn "Historico agregado"

--   -- Imprimir la lista de historicos actualizada
--   lista <- listaHistoricos
--   putStrLn "Lista de historicos:"
--   print lista

--     -- Agregar un historico y mostrar un mensaje de confirmación
--   setHistorico "1" "10" "35"
--   putStrLn "Historico agregado"

--   -- Imprimir la lista de historicos actualizada
--   lista <- listaHistoricos
--   putStrLn "Lista de historicos:"
--   print lista

  -- lista <- listaProfesores
  -- putStrLn "Lista de profesores:"
  -- print lista

  -- lista <- listaAlumnos
  -- putStrLn "Lista de alumnos:"
  -- print lista

  -- setAlumno "Alan"

  -- setProfesor "Jose"
    
  -- lista <- listaProfesores
  -- putStrLn "Lista de profesores:"
  -- print lista

  -- lista <- listaAlumnos
  -- putStrLn "Lista de alumnos:"
  -- print lista

  -- setMateria "INF3188" "Prog Logica"


  -- lista <- listaMaterias
  -- putStrLn "Lista de materias:"
  -- print lista





  -- -- Obtener y mostrar un historico específico por su registro
  -- historicos <- getHistoricosAlumno "30"
  -- putStrLn "El historico es: " 
  -- print historicos

  -- updateHistorico "30" "10" "100"
  -- putStrLn "Historico actualizado"

  -- lista <- listaHistoricos
  -- putStrLn "Lista de historicos:"
  -- print lista

  -- deleteHistorico "10" "30"
  -- putStrLn "Historico eliminado"

  -- lista <- listaHistoricos
  -- putStrLn "Lista de historicos:"
  -- print lista

  --------------------------------------------------------------------

  -- -- Agregar un materia y mostrar un mensaje de confirmación
  -- setGpMateria "MAT101" "25"
  -- putStrLn "Grupo Materia agregada"

  -- -- Imprimir la lista de Materias actualizada
  -- lista <- listaGpMaterias
  -- putStrLn "Lista de Grupo Materias:"
  -- print lista

  -- -- Agregar un materia y mostrar un mensaje de confirmación
  -- setGpMateria "INF318" "25"
  -- putStrLn "Grupo Materia agregada"

  -- -- Imprimir la lista de Materias actualizada
  -- lista <- listaGpMaterias
  -- putStrLn "Lista de Grupo Materias:"
  -- print lista

  -- -- Obtener y mostrar un materia específico por su registro
  -- gpMateria <- getGpMateria "2"
  -- putStrLn "El grupo materia es:"
  -- print gpMateria

  -- updateGpMateria "2" "INF110" "25"
  -- putStrLn "GpMateria actualizada"

  -- lista <- listaGpMaterias
  -- putStrLn "Lista de gpMaterias:"
  -- print lista

  -- deleteGpMateria "2"
  -- putStrLn "gpMateria eliminada"

  -- lista <- listaGpMaterias
  -- putStrLn "Lista de gpMaterias:"
  -- print lista

  ---------------------------------------------------------------

  -- -- Agregar un materia y mostrar un mensaje de confirmación
  -- setMateria "INF318" "Prog Logica"
  -- putStrLn "Materia agregada"

  -- -- Imprimir la lista de Materias actualizada
  -- lista <- listaMaterias
  -- putStrLn "Lista de Materias:"
  -- print lista

  --   -- Agregar un materia y mostrar un mensaje de confirmación
  -- setMateria "MAT101" "Calculo I"
  -- putStrLn "Materia agregada"

  -- -- Imprimir la lista de Materias actualizada
  -- lista <- listaMaterias
  -- putStrLn "Lista de Materias:"
  -- print lista

  -- -- Obtener y mostrar un materia específico por su registro
  -- materia <- getMateria "INF318"
  -- let materiaNombre = nombreMateria materia
  -- putStrLn $ "El materia es: " ++ materiaNombre

  -- updateMateria "MAT101" "MAT101" "Calculo II"
  -- putStrLn "Materia actualizada"

  -- lista <- listaMaterias
  -- putStrLn "Lista de materias:"
  -- print lista

  -- deleteMateria "MAT101"
  -- putStrLn "Materia eliminada"

  -- lista <- listaMaterias
  -- putStrLn "Lista de materias:"
  -- print lista

-----------------------------------------------------------

  -- -- Agregar un profesor y mostrar un mensaje de confirmación
  -- setProfesor "Jose"
  -- putStrLn "Profesor agregado"

  -- -- Imprimir la lista de profesores actualizada
  -- lista <- listaProfesores
  -- putStrLn "Lista de profesores:"
  -- print lista

  -- -- Obtener y mostrar un profesor específico por su registro
  -- profesor <- getProfesor "1"
  -- let profesorNombre = nombreProfesor profesor
  -- putStrLn $ "El profesor es: " ++ profesorNombre

  -- setProfesor "Pepe"
  -- putStrLn "Profesor agregado"

  -- lista <- listaProfesores
  -- putStrLn "Lista de profesores:"
  -- print lista

  -- updateProfesor "2" "Josue"
  -- putStrLn "Profesor actualizado"

  -- lista <- listaProfesores
  -- putStrLn "Lista de profesores:"
  -- print lista

  -- deleteProfesor "2"
  -- putStrLn "Profesor eliminado"

  -- lista <- listaProfesores
  -- putStrLn "Lista de profesores:"
  -- print lista

-------------------------------------------------------------------

--   -- Agregar un alumno y mostrar un mensaje de confirmación
--   setAlumno "Alan"
--   putStrLn "Alumno agregado"

--   -- Imprimir la lista de alumnos actualizada
--   lista <- listaAlumnos
--   putStrLn "Lista de alumnos:"
--   print lista

  -- -- Obtener y mostrar un alumno específico por su registro
  -- alumno <- getAlumno "1"
  -- let alumnoNombre = nombreAlumno alumno
  -- putStrLn $ "El alumno es: " ++ alumnoNombre

--   setAlumno "Maria"
--   putStrLn "Alumno agregado"

--   lista <- listaAlumnos
--   putStrLn "Lista de alumnos:"
--   print lista

  -- updateAlumno "2" "Lucia"
  -- putStrLn "Alumno actualizado"

  -- lista <- listaAlumnos
  -- putStrLn "Lista de alumnos:"
  -- print lista

--   deleteAlumno "1"
--   putStrLn "Alumno eliminado"

--   lista <- listaAlumnos
--   putStrLn "Lista de alumnos:"
--   print lista

--   -- Imprimir la lista de historicos actualizada
--   lista <- listaHistoricos
--   putStrLn "Lista de historicos:"
--   print lista




