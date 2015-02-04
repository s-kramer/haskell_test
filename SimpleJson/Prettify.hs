import SimpleJson
import PrettyStub

renderJValue :: JValue -> Doc
renderJValue (JString s) = string s
renderJValue (JBool False) = text "false"
renderJValue (JBool True) = text "true"
renderJValue (JNull) = text "null"
renderJValue (JNumber n) = double n


