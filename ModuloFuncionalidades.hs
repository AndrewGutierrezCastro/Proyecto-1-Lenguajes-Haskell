module ModuloFuncionalidades where 
import Data.Map (Map, lookup, fromList, (!), member)

type Line = [Token]
type HypMap = Data.Map.Map String [String]
data Token = Word String | Blank | HypWord String 
                deriving (Eq, Show)
data Separacion = SEPARAR | NOSEPARAR
data Ajuste = AJUSTAR | NOAJUSTAR

string2Line :: String -> Line
string2Token :: String -> Token
tokenLength :: Token -> Int
lineLength :: Line -> Int
breakLine :: Int -> Line -> (Line,Line)
mergers :: [String] -> [(String, String)] 
enHyp :: HypMap
hyphenate :: HypMap -> Token -> [(Token,Token)]
lineBreaks :: HypMap -> Int -> Line -> [(Line,Line)]
insertBlanks :: Int -> Line -> Line

enHyp = Data.Map.fromList [ ("controla",["con","tro","la"]), 
                            ("futuro",["fu","tu","ro"]),
                            ("presente",["pre","sen","te"])]

--String2Line esta funcion convierte una tira en words
-- >>string2Line "hola que tal"
-- [Word "hola",Word "que",Word "tal"]
string2Line str =  map string2Token (words str)

string2Token str = Word str

--Line2String convierte un Line en una tira string
--en medio de cada token agrega un espacio
line2String line = concat (map token2String line)

token2String (Word wrd) = wrd
token2String (Blank) = " "
token2String (HypWord hpw) = hpw++"-" 

lineLength line
    |  lgth > 1 = sum (map tokenLength line) + ((length line)-1)
    |  lgth == 1 = tokenLength (head line)
    | otherwise = 0

    where lgth = length line


tokenLength (Word wrd) = length wrd
tokenLength (Blank) = 1
tokenLength (HypWord hypWrd) = length hypWrd + 1

breakLine n (l:lines) = if largo <= n 
                            then (l : fst cumplen, snd cumplen )
                        else
                            ([],l:lines)
                        where largo = (tokenLength l)
                              cumplen = (breakLine (n-largo) lines)

breakLine _ ([]) = ([],[])
--Mergers
--
--
--
--

mergers (s:strs) =  if strs /= []
                        then  combinacion : mergers ( cabeza:cola)
                    else 
                        []
                    where cabeza = (s++(head strs))
                          cola =  (tail strs)
                          combinacion = (s, concat strs)
--Hyphenate
hyphenate mapa (Word word) = if (member sinPuntos mapa)
                                then map (\(x, y) -> (HypWord x, Word (y++puntos) )) (mergers (mapa ! sinPuntos))
                            else 
                                []
                            where
                                sinPuntos = (sacarPuntos word)
                                puntos = drop (length sinPuntos) word 
--auxiliar de hyphenate
sacarPuntos word = takeWhile (\str->str /= '.') word

--Linebreaks
lineBreaks mapa n line = takeWhile (\x -> (lineLength (fst x) ) <= n) listaCompleta
    where listaCompleta = 
            if  (length line) == 1
                then [((take 1 line),[])] 
            else 
                (inicioLista, [last line]) : map lineBreaksAux (hyphenate enHyp (last line) ) 
          inicioLista = (init line) 
          lineBreaksAux tupla = (inicioLista ++ [(fst tupla)], [(snd tupla)])  

--InsertBlanks 
insertBlanks n (x:xs)
    | (length (x:xs)) == 1 || n <= 0 = x:xs
    | n > 0 = x:(replicate numBlank Blank)  ++ (insertBlanks (n-numBlank) xs)
    where numBlank  = ceiling( div :: Double)
          div =  ( (fromIntegral n) / (fromIntegral (length xs)) ) 

insertBlanksAux line listBlank
    | listBlank == [] = line
    | otherwise = (head line):(head listBlank) ++ insertBlanksAux (tail line) (tail listBlank)
    


separarYalinear n NOSEPARAR NOAJUSTAR tira estado = map line2String (pegadoSinAjuste n (string2Line tira) [] [] estado) 
separarYalinear n NOSEPARAR AJUSTAR tira estado = map line2String (pegadoConAjuste n (string2Line tira) [] [] estado)
separarYalinear n SEPARAR NOAJUSTAR tira estado = map line2String (separadoSinAjuste n (string2Line tira) [] [] estado)
separarYalinear n SEPARAR AJUSTAR tira estado = map line2String (separadoConAjuste n (string2Line tira) [] [] estado) 


--Esta funcion resive el n de largo maximo, una lista de tokens una lista 
--final y una lista acumulada para ir creando cada linea
pegadoSinAjuste n (x:xs) lstFinal lstAcum estado
    | largoLstAcm <= n = pegadoSinAjuste n xs lstFinal (lstAcum++[x]) estado
    | largoLstAcm > n = pegadoSinAjuste n xs (lstFinal++[(insertBlanks numEspacios lstAcum)]) [x] estado
    
    where largoLstAcm = ((lineLength lstAcum )+(tokenLength x))
          numEspacios = (length lstAcum)-1
    --el largo de la lista acumulada + el largo del siguiente token

--Condicion de parada
pegadoSinAjuste _ [] lstFinal lstAcum  estado = lstFinal++[(insertBlanks numEspacios lstAcum)]
    where numEspacios = (length lstAcum)-1
--Esta funcion
--
pegadoConAjuste n (x:xs) lstFinal lstAcum estado
    | largoLstAcm < n = pegadoConAjuste n xs lstFinal (lstAcum++[x]) estado
    | largoLstAcm > n = pegadoConAjuste n xs (lstFinal++[(insertBlanks (n-largoLst+numEspacios) lstAcum)]) [x] estado
    | largoLstAcm == n = pegadoConAjuste n xs ( lstFinal++[(insertBlanks numEspacios lstAcum)] ) [x] estado
    
    where largoLstAcm = (largoLst +(tokenLength x) + 1) 
          largoLst = lineLength lstAcum 
          numEspacios = (length lstAcum)-1 
    --el largo de la lista acumulada + el largo del siguiente token 
pegadoConAjuste n [] lstFinal lstAcum estado =  (lstFinal++[(insertBlanks (n-largoLstAcm+numEspacios) lstAcum)])
    where largoLstAcm = (lineLength lstAcum)  
          numEspacios = (length lstAcum)-1
    --el largo de la lista acumulada + el largo del siguiente token 

--SEPARADOS SIN AJUSTE
separadoSinAjuste n [] lstFinal lstAcum _ = (lstFinal++[(insertBlanks numEspacios lstAcum)])
    where largoLstAcm = (lineLength lstAcum)  
          numEspacios = (length lstAcum)-1

separadoSinAjuste n (x:xs) lstFinal lstAcum estado
    | largoLstAcm < n = separadoSinAjuste n xs lstFinal (lstAcum++[x]) estado
    -- | largoLstAcm == n = separadoSinAjuste n xs (lstFinal++[(insertBlanks (n-largoLstAcm+numEspacios) lstAcum++[x])]) [] estado
   
    where largoLstAcm = (largoLst + (tokenLength x)) 
          largoLst = lineLength lstAcum
          numEspacios = (length lstAcum)-1
    --el largo de la lista acumulada + el largo del siguiente token 
    --el largo de la lista acumulada
    --el numero de espacios en la lista acumulada

separadoSinAjuste n (x:xs) lstFinal lstAcum estado
     | largoLstAcm >= n = separadoSinAjuste n xs (lstFinal++[(insertBlanks numEspacios lstSeparada)]) (snd wrdSep) estado

    where largoLstAcm = (largoLst +(tokenLength x)) 
          largoLst = lineLength lstAcum
          lstSeparada = (lstAcum++(fst wrdSep))
          wrdSep = separadoAux x n largoLst estado --([HypWord x], [Word y])
          numEspacios = (length lstSeparada)-1
    --el largo de la lista acumulada + el largo del siguiente token 
    --el largo de la lista acumulada
    --la cantidad de espacios en la lista separada
    --palabra separada es una tupla con x elemento con guion y el resto en y

--Esta funcion recibe un token Word que se desea separar al maximo pero sin superar el largo n
separadoAux x n largoLst estado = 
    if tupla == ([],[])
        then ([], [x])
    else
        tupla
    where lista = hyphenate estado x
          tupla = (maximaSeparacion lista n (largoLst+1) [])
          --lista de separaciones| maximo total| largo lista  mas un espacio| lista vacia

maximaSeparacion (x:xs) n largoLst (y:yz)
    | (tokenLength (fst x))+largoLst < n = maximaSeparacion xs n largoLst ([(fst x), (snd x)])
    | (tokenLength (fst x))+largoLst == n = ([(fst x)], [(snd x)])
    | (tokenLength (fst x))+largoLst > n = ([y], yz)

maximaSeparacion (x:xs) n largoLst [] 
    | (tokenLength (fst x))+largoLst <= n = maximaSeparacion xs n largoLst ([(fst x), (snd x)])
    | otherwise =  ([],[])

maximaSeparacion [] _ _ []  = ([],[])
maximaSeparacion [] _ _ (x:xs) = ([x], xs) 

separadoConAjuste n [] lstFinal lstAcum _ = (lstFinal++[(insertBlanks (n-largoLstAcm+numEspacios) lstAcum)])
    where largoLstAcm = (lineLength lstAcum)  
          numEspacios = (length lstAcum)-1

separadoConAjuste n (x:xs) lstFinal lstAcum estado
    | largoLstAcm < n = separadoConAjuste n xs lstFinal (lstAcum++[x]) estado
  --  | largoLstAcm == n = separadoConAjuste n xs (lstFinal++[(insertBlanks (n-largoLstAcm+numEspacios) (lstAcum++[x]))]) [] estado
   
    where largoLstAcm = (largoLst + (tokenLength x)) 
          largoLst = lineLength lstAcum
          numEspacios = (length lstAcum)-1
    --el largo de la lista acumulada + el largo del siguiente token 
    --el largo de la lista acumulada
    --el numero de espacios en la lista acumulada

separadoConAjuste n (x:xs) lstFinal lstAcum estado
     | largoLstAcm >= n = separadoConAjuste n xs (lstFinal++[(insertBlanks (n-largoSprd+numEspacios) lstSeparada)]) (snd wrdSep) estado

    where largoLstAcm = (largoLst +(tokenLength x)) 
          largoLst = lineLength lstAcum
          lstSeparada = (lstAcum++(fst wrdSep))
          largoSprd = lineLength lstSeparada 
          wrdSep = separadoAux x n largoLst estado --([HypWord x], [Word y])
          numEspacios = (length lstSeparada)-1
    --el largo de la lista acumulada + el largo del siguiente token 
    --el largo de la lista acumulada
    --la cantidad de espacios en la lista separada
    --palabra separada es una tupla con x elemento con guion y el resto en y