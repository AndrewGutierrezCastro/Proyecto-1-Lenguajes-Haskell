type Line = [Token]
data Token = Word String | Blank String| HypWord String 
                deriving (Eq, Show)

main :: IO ()

string2Line :: String -> Line
string2Token :: String -> Token

main = putStrLn "Hello, World!"

string2Line str =  map string2Token (words str)

string2Token str = Word str

line2String line = unwords (map token2String line)

token2String (Word tkn) = tkn
token2String (Blank blk) = " "
token2String (HypWord hpw) = hpw 