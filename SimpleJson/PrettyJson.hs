module PrettyJson where 

import PrettyStub

enclose :: Char -> Char -> Doc -> Doc
enclose left right core = char left <> core <> char right

