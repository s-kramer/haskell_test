-- module PrettyJson where 
import PrettyStub

enclose :: Char -> Char -> Doc -> Doc
enclose left right before = char left <> before <> char right

