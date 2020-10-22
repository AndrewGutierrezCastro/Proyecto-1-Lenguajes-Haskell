type Line = [Token]
data Token = Word String | Blank Char | HypWord String 
                deriving (Eq, Show)

main :: IO ()
inc :: (Num a) => a -> a
string2Line :: String -> Line
string2Token :: String -> Token

main = putStrLn "Hello, World!"

inc a = a + 1

sumar a b = a + b 

potencia a b = a ^ b 

maximoStr a b = if length a >= length b 
                then a
                else b 

string2Line str =  [string2Token str]

string2Token str = Word str


