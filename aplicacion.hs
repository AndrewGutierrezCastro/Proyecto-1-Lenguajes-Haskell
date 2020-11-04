import Prelude hiding (null, lookup, map, filter)
import Data.Map.Lazy hiding (sort,map,foldl)
import Data.Char
import Data.List (sort,map)
import System.IO
import Data.List.Split
import ModuloFuncionalidades
-- El Estado es una "Map" (hash String -> Int) que
-- para cada palabra da el número de veces que la ha encontrado
type Estado = Map [Char] [String]

-- main crea un Estado vacío e invoca a mainloop
-- el cual recibe el Estado como parámetro
main :: IO ()
main = do 
       mainloop (fromList[])

-- Ciclo de ejecución:
--     recibe un Estado
--     lee un comando
--     ejecuta un comando que produce un nuevo Estado
--     se invoca recursivamente con el nuevo Estado.
mainloop :: Estado -> IO ()
mainloop estado = do
  putStr ">> "
  inpStr <- getLine
  let tokens  = words inpStr
  let comando = tokens!!0
  
  case comando of
     "leer" -> do
               putStrLn ">>> Nombre archivo entrada: "
               nombreArchivo <- getLine
               inh <- openFile nombreArchivo ReadMode
               nuevoestado <- cargar inh estado
               hClose inh
               putStrLn $ "Archivo " ++ nombreArchivo ++ " fue cargado"
               mainloop nuevoestado
               
     "guardar" -> do
               putStrLn ">>> Nombre archivo salida: "
               nombreArchivo <- getLine
               outh <- openFile nombreArchivo WriteMode
               descargar outh (sort (toList estado))
               hClose outh
               mainloop estado  

     "split"  -> do
               let lngth = read (tokens!!1) :: Int
                   sprar = tokens!!2
                   ajst = tokens!!3
                   tira = tokens!!4
               let str = splitFnt lngth sprar ajst tira
               putStrLn str            
               mainloop estado

     "clsDic" -> do 
               putStr ">> Diccionario Limpio"
               mainloop (fromList[]) 

     "imp" -> do
                 let (nuevoestado, salida) = cmd_imp estado
                 putStrLn salida
                 mainloop nuevoestado
     "fin" -> do
                 putStrLn "Saliendo..."
     _     -> do
                 putStrLn $ "Comando desconocido ("++ comando ++"): '" ++ inpStr ++ "'" 
                 mainloop estado

-- función que implementa leer un archivo línea por línea
-- y contar las palabras de cada línea
cargar :: Handle -> Estado -> IO Estado
cargar inh estado = do
      ineof <- hIsEOF inh
      if ineof then return estado
               else do inpStr <- hGetLine inh
                       let nuevoestado = ins estado inpStr
                       cargar inh nuevoestado

ins :: Estado -> String -> Estado
ins estado str = insert key wrds estado
    where key = head (splitOn " " str) 
          wrds = (splitOn "-" (last (splitOn " " str)))

-- función que implementa el comando imp
cmd_imp :: Estado -> (Estado, String)
cmd_imp estado = (estado, show estado)

-- descargar :: Handle -> [(String,Int)] -> IO ()
descargar outh [] = return ()
descargar outh ((k,v):kvs) = do hPutStrLn outh $ k ++ " " ++ (show v)
                                descargar outh kvs

--funcion que implementa el comando Split
splitFnt :: Int -> String -> String -> String ->[String]                             
splitFnt n "n" "n" tira = separarYalinear n NOSEPARAR NOAJUSTAR tira
splitFnt n "n" "s" tira = separarYalinear n NOSEPARAR AJUSTAR tira 
splitFnt n "s" "n" tira = separarYalinear n SEPARAR NOAJUSTAR tira 
splitFnt n "s" "s" tira = separarYalinear n SEPARAR AJUSTAR tira


