type Line = [Token]
data Token = Word String | Blank | HypWord String 
                deriving (Eq, Show)

main :: IO ()

string2Line :: String -> Line
string2Token :: String -> Token
tokenLength :: Token -> Int
lineLength :: Line -> Int
breakLine :: Int -> Line -> (Line,Line)
mergers :: [String] -> [(String, String)] 
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

mergers (s:strs) = if strs /= []
                        then  combinacion : mergers ( cabeza: cola)
                    else 
                        []
                    where cabeza = (s++(head strs))
                          cola =  (tail strs)
                          combinacion = (s, concat strs)

 