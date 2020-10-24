import Data.Map (Map, lookup, fromList, (!), member)

type Line = [Token]
type HypMap = Data.Map.Map String [String]
data Token = Word String | Blank | HypWord String 
                deriving (Eq, Show)

main :: IO ()

string2Line :: String -> Line
string2Token :: String -> Token
tokenLength :: Token -> Int
lineLength :: Line -> Int
breakLine :: Int -> Line -> (Line,Line)
mergers :: [String] -> [(String, String)] 
enHyp :: HypMap
hyphenate :: HypMap -> Token -> [(Token,Token)]
lineBreaks :: HypMap -> Int -> Line -> [(Line,Line)]


enHyp = Data.Map.fromList [ ("controla",["con","tro","la"]), 
                            ("futuro",["fu","tu","ro"]),
                            ("presente",["pre","sen","te"])]


main = putStrLn "Hello, World!"

string2Line str =  map string2Token (words str)

string2Token str = Word str

line2String line = unwords (map token2String line)

token2String (Word wrd) = wrd
token2String (Blank) = " "
token2String (HypWord hpw) = hpw++"-" 

lineLength line = sum (map tokenLength line)

tokenLength (Word wrd) = length wrd
tokenLength (Blank) = 1
tokenLength (HypWord hypWrd) = length hypWrd

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
--
--
--
--
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
--
--
--
--
lineBreaks mapa n line = takeWhile (\x -> (lineLength (fst x) ) <= n) listaCompleta
    where listaCompleta = 
            if  (length line) == 1
                then [((take 1 line),[])] 
            else 
                (inicioLista, [last line]) : map lineBreaksAux (hyphenate enHyp (last line) ) 
          inicioLista = (init line) 
          lineBreaksAux tupla = (inicioLista ++ [(fst tupla)], [(snd tupla)])  

