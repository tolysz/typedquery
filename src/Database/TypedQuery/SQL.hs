
module Database.TypedQuery.SQL where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Prelude

sql :: TokenParser st
sql = makeTokenParser sqlDef

sqlExpr :: TokenParser st
sqlExpr = makeTokenParser sqlDefExpr

sqlExpr2 :: TokenParser st
sqlExpr2 = makeTokenParser sqlDefExpr2

sqlStyle :: LanguageDef st
sqlStyle = emptyDef
                { commentStart   = "/*"
                , commentEnd     = "*/"
                , commentLine    = "-- #"
                , nestedComments = True
                , identStart     = letter
                , identLetter    = alphaNum <|> oneOf "_.[]"
                , opStart        = opLetter sqlStyle
                , opLetter       = oneOf "$%&*+./<=>?@\\^|-~()"
                , reservedOpNames= []
                , reservedNames  = []
                , caseSensitive  = False
                }

sqlStyleCS :: LanguageDef st
sqlStyleCS = sqlStyle {
       caseSensitive  = True
       }

sqlDefExpr :: LanguageDef st
sqlDefExpr = sqlStyleCS
       { identLetter    = alphaNum <|> oneOf "_.[]"
       , reservedNames  = [ "as", "AS", "As", "aS"
                   , "from", "FROM", "fROM", "From"
                   ]
}

sqlDefExpr2 :: LanguageDef st
sqlDefExpr2 = sqlStyleCS
       { identLetter    = alphaNum <|> oneOf "_.[]"
       , reservedNames  = [ "as", "AS", "As", "aS"
                          , "from", "FROM", "fROM", "From"
                          ]
       }

sqlDef :: LanguageDef st
sqlDef = sqlStyle
                { reservedOpNames= ["=","\\","|","<",">","+","-","*","@",">=","<="]
                , reservedNames  = ["AS", "LEFT", "FROM", "SELECT", "INSERT", "DISTINCT", "UNIQUE"
                                   , "CONCAT", "CONCAT_WS", "NOW", "COUNT", "CASE", "WHEN", "THEN", "IF", "ELSE", "NULL"
                                   , "CONVERT_TZ","GROUP_CONCAT", "SUM", "JOIN", "END", "DATE", "TIME_TO_SEC", "TIMEDIFF"
                                   , "AVG", "MIN", "MAX", "GROUP", "BY", "LIMIT", "ORDER"
                                   , "IFNULL", "IN"]
                }
