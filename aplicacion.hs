import Prelude hiding (null, lookup, map, filter)
import Data.Map.Lazy hiding (sort,map,foldl,drop)
import Data.Char
import Data.List (sort,map,intercalate)
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
     "load" -> do
               let nombreArchivo = tokens!!1
               inh <- openFile nombreArchivo ReadMode
               nuevoestado <- cargar inh estado
               hClose inh
               putStrLn $ "Diccionario cargado ("++(show(size nuevoestado))++" palabras)"
               mainloop nuevoestado
     "save" -> do 
               let nombreArchivo = tokens!!1
               outh <- openFile nombreArchivo WriteMode
               descargar outh (sort (toList estado))
               hClose outh
               putStrLn $ "Diccionario guardado ("++(show(size estado))++" palabras)"
               mainloop estado 
     "ins" -> do
              let tira = unwords (drop 1 tokens) 
              let nuevoEstado = ins estado tira
              mainloop nuevoEstado
     -- >> split 20 n s Quien controla …
     "split" -> do 
                let lngth = (read (tokens!!1) :: Int)
                    sprar = tokens!!2
                    ajst = tokens!!3
                    tira = unwords (drop 4 tokens) 
                
                let str = splitFnt lngth sprar ajst tira estado        
                mapM_ putStrLn str         
                mainloop estado

     -- >> splitf 20 n s prueba1.txt
     "splitf" -> do 
                let lngth = (read (tokens!!1) :: Int)
                    sprar = tokens!!2
                    ajst = tokens!!3
                    arch1 = tokens!!4 

                inh <- openFile arch1 ReadMode
                strRead <- cargarTxt inh ""
                hClose inh

                let str = splitFnt lngth sprar ajst strRead estado  
                mapM_ putStrLn str   
                let arch2 = unwords (drop 5 tokens)
                guardado <- guardar arch2 str
                mainloop estado
                
     "clsDic" -> do 
               putStrLn ">> Diccionario Limpio"
               mainloop (fromList[]) 

     "show" -> do
                 let (nuevoestado, salida) = cmd_imp estado
                 putStrLn salida
                 mainloop nuevoestado
     "exit" -> do
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
cmd_imp estado = (estado, show (toList estado))

-- descargar :: Handle -> [(String,Int)] -> IO ()
{- guardar outh [] = return ()
guardar outh (x:xs) = do hPutStrLn outh $ x
                                descargar outh kvs -}

--funcion que implementa el comando Split
splitFnt :: Int -> String -> String -> String -> Estado ->[String]                             
splitFnt n "n" "n" tira estado = separarYalinear n NOSEPARAR NOAJUSTAR tira estado
splitFnt n "n" "s" tira estado = separarYalinear n NOSEPARAR AJUSTAR tira estado
splitFnt n "s" "n" tira estado = separarYalinear n SEPARAR NOAJUSTAR tira estado 
splitFnt n "s" "s" tira estado = separarYalinear n SEPARAR AJUSTAR tira estado 


--funcion que implementa la lectura de un archivo txt 
cargarTxt inh str = do
      ineof <- hIsEOF inh
      if ineof then return str
               else do inpStr <- hGetLine inh
                       cargarTxt inh (str++inpStr)

guardarTxt outh [] = return ()
guardarTxt outh (s:xs) = do hPutStrLn outh s
                            guardarTxt outh xs

guardar arch2 str
    | ((length (arch2)) < 4) || (str == []) = return ()
    | otherwise = do outh <- openFile arch2 WriteMode
                     guardarTxt outh str
                     hClose outh

descargar outh [] = return ()
descargar outh ((palabra,silabas):lst) = do 
    hPutStrLn outh ( palabra ++ " " ++ (intercalate "-" silabas) )
    descargar outh lst

