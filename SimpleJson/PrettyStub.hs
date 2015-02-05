module PrettyStub where
import SimpleJson
import PrettyJson

data Doc = ToBeDefined
  deriving (Show)

text :: String -> Doc
text t = undefined

double :: Double -> Doc
double d = undefined

string :: String -> Doc
string s = enclose "'" "'" . hcat . map oneChar

char :: Char -> Doc
char c = undefined

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined
