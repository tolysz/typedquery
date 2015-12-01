
{-# Language OverloadedStrings #-}
{-# Language DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module Database.TypedQuery.SQLParser
(typedSQLtoSQL, TypeAction(..))
where

import Data.Char ( toUpper)
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Typeable (Typeable)

import Text.Parsec hiding ((<|>))
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Char
import Database.TypedQuery.SQL
import Control.Applicative hiding (many, optional)

--import Control.Arrow (first)

import Control.Monad -- (void, foldM_)
import Data.Monoid (Monoid (..))
import qualified Language.Haskell.TH.Syntax as TH

import Data.Maybe
import qualified Data.List as DL
import Prelude

#if DEVELOPMENT
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)

pprint :: SQLWriter -> IO ()
pprint (SQLWriter a b c d zz) = {- putStrLn a >> -} print zz >> print (zip b c) >> print d
#endif


data TypeAction
-- | SELECT
  = TAUnknown
-- ^ column of unknown type
  | TACast String
-- ^ column type
  | TAConv String
-- ^ column conversion function
  | TAInUnknown
-- ^ `?` input of unknown type
  | TAInCast String
-- ^ `?` input of known type
  | TAInConv String
-- ^ `?` conversion function
  | TAInSupply String
-- ^ `?` supply SQL literal string
-- | INSERT
  | TAInsNop
-- ^ do nothing
  | TAInsQM
-- ^ insery QuestionMark
  | TAInsQMU
-- ^ 
  | TAInsLit String
  | TAInsIS  String
  | TAInsISU  String
  deriving (Eq, Show, Ord, Typeable)


data SQLWriter
-- ^ type to store data while we parse the query
 = SQLWriter
   { queryW  :: String
-- ^ represents output sql
   , nameW   :: [String]
-- ^ stores column names
   , typeW   :: [TypeAction]
-- ^ stores column types
   , inTypeW :: [TypeAction]
-- ^ stores '?' types
   , inTypeV :: [TypeAction]
-- ^ stores output
   } deriving (Show)
instance TH.Lift TypeAction where
  lift = TH.lift



toValu :: TypeAction -> Maybe String
toValu TAInsQM      = Just "?"
toValu (TAInsIS _)  = Just "?"
toValu (TAInsLit s) = Just s
toValu _ = Nothing

filTAInsLit :: TypeAction -> Bool
filTAInsLit (TAInsLit _) = False
filTAInsLit _ = True

-- (catMaybes $ map toValu z) 
addValues :: [String] -> String
addValues [] = ""
addValues a = " VALUES ( "++ DL.intercalate " , " a ++" );"


fromSQLWriter :: SQLWriter -> (String, [Text], [TypeAction], [TypeAction], [TypeAction])
fromSQLWriter (SQLWriter a b c d zz) = (a ++ addValues (mapMaybe toValu zz), map T.pack b, c, d,  filter filTAInsLit zz)

type WParser a = ParsecT String SQLWriter Identity a

addW :: SQLWriter -> WParser ()
addW a = updateState (`mappend` a)

addU, addIU, addJU :: WParser ()
addU    = addW $ mempty { typeW   = [TAUnknown]   }
addIU   = addW $ mempty { inTypeW = [TAUnknown]   }
addJU   = addW $ mempty { inTypeW = [TAInUnknown] }

addKU, addKQM, addKQMU :: WParser ()
addKU   = addW $ mempty    { inTypeV = [TAInsNop]   }
addKQM  = addW $ mempty   { inTypeV = [TAInsQM]    }
addKQMU = addW $ mempty   { inTypeV = [TAInsQMU] }

addKLit, addKIS, addKISU :: String -> WParser ()
addKLit s = addW $ mempty { inTypeV = [TAInsLit s] }
addKIS  s = addW $ mempty  { inTypeV = [TAInsIS s]  }
addKISU s = addW $ mempty  { inTypeV = [TAInsISU s]  }

addQ1c :: Char -> WParser ()
addQ1c s = addW $ mempty { queryW  = [s]     }

-- addQ1c1 s = addW $ mempty { queryW  = ' ':s:" " }

addQ, addN, addQQ, addQ1, addQR, addQN, addT, addC, addIT, addIC, addJT, addJC :: String -> WParser ()
addQ  s = addW $ mempty { queryW  = ' ':s }
addN  s = addW $ mempty { nameW   = [s] }
addQN s = addW $ mempty { queryW  = ' ':s , nameW   = [s] }

addQ1 s = addW $ mempty { queryW  = s     }
addQR s = addW $ mempty { queryW  = ' ':s++" "   }
addQQ s = addW $ mempty { queryW  = " \""++s++"\" "   }
addT  s = addW $ mempty { typeW   = [TACast s] }
addC  s = addW $ mempty { typeW   = [TAConv s] }
addIT s = addW $ mempty { inTypeW = [TACast s] }
addIC s = addW $ mempty { inTypeW = [TAConv s] }

addJT s = addW $ mempty { inTypeW = [TAInCast  s] }
addJC s = addW $ mempty { inTypeW = [TAInConv  s] }

addJISU :: String -> WParser ()
addJISU s  = addW $ mempty   { inTypeV = [TAInSupply s]}

instance Monoid SQLWriter where
  mempty = SQLWriter mempty mempty mempty mempty mempty
  SQLWriter a1 a2 a3 a4 a5 `mappend` SQLWriter b1 b2 b3 b4 b5
        = SQLWriter (mappend a1 b1) (mappend a2 b2) (mappend a3 b3) (mappend a4 b4) (mappend a5 b5)

typedSQLtoSQL :: String -> Maybe (String, [Text], [TypeAction], [TypeAction], [TypeAction])
-- ^ takes some annotates string and gives its parsed representation
typedSQLtoSQL a =
            case runParser (mksql >> getState) mempty "nope" a of
              Right r -> let
                             rr = fromSQLWriter r
                         in
#if DEVELOPMENT
                         unsafePerformIO $ do
                              putStrLn $ (\(a1,_,_,_,_) -> '\n':a1) rr
                              pprint r
                              liftIO $ return $ Just rr
#else
                         Just rr
#endif
              Left l -> error $ show l
-- *
mksql :: WParser ()
mksql = try parseSelect <|> try parseInsert <|> (do optional (try typeParams);addQ =<< getInput)

-- eol  :: WParser ()
-- *
eol :: WParser ()
eol = try (void $ string "\r\n")
  <|> try (void $ string "\n\r")
  <|> try (void $ char '\r')
  <|> try (void $ char '\n')
  <|> notFollowedBy anyToken

-- *
caseChar :: Char -> WParser Char
caseChar c = satisfy (\x -> toUpper x == toUpper c)

-- *
stringSQL :: String -> WParser String
stringSQL cs = lexeme sql ((mapM_ caseChar cs >> return cs) <?> cs)

-- *
wsKill :: WParser a -> WParser a
wsKill p = do
           whiteSpace haskell
           r <- p
           whiteSpace sql
           return r

-- *
rawHaskell :: WParser String
rawHaskell = wsKill $ manyTill anyChar eol

-- *
rawHaskellT :: WParser String
rawHaskellT = wsKill $ manyTill anyChar (eol <|> void (lookAhead $ try $ string "-- <"))

-- *
stringLiteralQ :: WParser String
stringLiteralQ  = do
                  lit <- stringLiteral haskell
                  return $ show lit

stringLiteralSQL = lexeme sql (
          do{ str <- between (char '\'')
                             (char '\'' <?> "end of string")
                             (many stringChar)
            ; return (foldr (maybe id (:)) "" str)
            }
          <?> "literal string")

stringChar      =   do{ c <- stringLetter; return (Just c) }
                    <|> stringEscape
                    <?> "string character"
stringLetter    = satisfy (/= '\'')
stringEscape    = string "\\'" *> return (Just '\'')

identifier1,identifier2 :: WParser String
identifier1 = try stringLiteralQ
          <|> try(greservedQ "NULL")
          <|> try (show <$> lexeme sql (integer sql))
          <|> lexeme sql (identifier sqlExpr) -- <|> ( manyTill anyChar (lexeme haskell $ lookAhead $ try $ reserved sql "as"))

singleQ  :: WParser ()
singleQ  = void $ char '\''


showSQ :: String -> String
showSQ = DL.intercalate "\\''" . go
  where
    go :: String -> [String]
    go  ""  =  []
    go s =  cons (case DL.break (== '\'') s of
                 (l, s') -> (l, case s' of
                     []      -> []
                     _:s''   -> go s''))
    cons ~(h, t)        =  h : t

identifier2 = try (quoteS . showSQ <$> stringLiteralSQL)
--           <|> try (lexeme sql $ identifier sqlExpr)
          <|> identifier1
           --  <|> expandQ (reservedNames sqlDef)

-- *
arithm :: WParser String
arithm = do
      whiteSpace sql
      r <- oneOf "%&*+/<=>-~"
      whiteSpace sql
      return [r]

-- *
identifier3 :: WParser String
identifier3 = 
       try (stringLiteral sql )
        <|> try ( do
             void . lexeme sql . string $ "("
             ret <- identifier3
             void . lexeme sql . string $ ")"
             return $ braketS ret
          ) <|> try (lexeme sql $ identifier sqlExpr2)
        --  <|> expandQ (reservedNames sqlDef)

-- *
greserved :: String -> WParser ()
greserved s = lexeme sql (reserved sql s) *> addQ s

greservedQ :: String -> WParser String
greservedQ s = lexeme sql (reserved sql s) >> return s

-- *
expand :: [String] -> WParser ()
expand = foldr ( (<|>) . try . greserved ) empty
-- expand [] = empty

-- *
expandQ :: [String] -> WParser String
expandQ  = foldr ( (<|>) . try . greservedQ ) (fail "not a reserved")
-- try (greservedQ a) <|> expandQ as
-- expandQ [] = fail "not a reserved"

-- *
typeParams :: WParser ()
typeParams = do
         try typeParamsSimple
             <|> try typeParamsIn
             <|> expand (reservedNames sqlDef)
             <|> try (addQ =<< lexeme sql (identifier sql))
             <|> try (addQ . show =<< integer sql)
             <|> try (addQQ =<< stringLiteral sql)
             <|> (addQ1c =<< anyChar)
         optional (try typeParams)

-- *
typeParamsIn :: WParser ()
typeParamsIn = do
             greserved "IN"
             addQR =<< stringSQL "?"
             try (
                 do
                   addKQMU
                   void $ string "-- >"
                   addJC =<< rawHaskell
                   addQ1 "\n      "
                 ) <|> try ( do
                 void $ string "-- <"
                 addJISU =<< rawHaskell
                 addIU
                  ) <|> try (
                 do
                   addKQMU
                   void $ string "--"
                   addJT =<< rawHaskell
                   addQ1 "\n      "
                 ) <|> addJU

-- *
optInputSourceU :: WParser ()
optInputSourceU = try ( do
         void $ string "-- <"
         addKISU =<< rawHaskell
       ) <|> addKQMU

-- *
typeParamsSimple :: WParser ()
typeParamsSimple = do
             addQR =<< stringSQL "?"
             try (
                 do
                   void $ string "-- >"
                   addIC =<< rawHaskellT
                   optInputSourceU
                   addQ1 "\n      "
                 ) <|> try ( do
                 void $ string "-- <"
                 addKISU =<< rawHaskell
                 addIU
                  )  <|> try (
                 do
                   void $ string "--"
                   addIT =<< rawHaskellT
                   optInputSourceU
                   addQ1 "\n      "
                 ) <|> (addIU >> addKQMU)

-- numericValueExpression :: WParser String
-- numericValueExpression = do

ls = lexeme sql

functionExpr :: WParser String
functionExpr = do
  fn <- lexeme sql identifierExpr
  body <- parens sql (commaSep sql commonValueExpression )
  return $ unwords ((fn ++ "(") : DL.intersperse ", " body)  ++ ")"

identifierExpr :: WParser String
identifierExpr = lexeme sql identifier2

-- todo: fix
commonValueExpression :: WParser String
commonValueExpression = do
  idt1 <- try ((++) <$> lexeme sql prefOps
                    <*> ((:) ' ' <$> commonValueExpression))
      <|> try (braketS <$> parens sql commonValueExpression)
      <|> try functionExpr
      <|> identifierExpr

  maybe idt1 ((idt1 ++ " ") ++) <$> optionMaybe (
     (++) <$> lexeme sql joinOps
          <*> ((:) ' ' <$> commonValueExpression)
     )


prefOps = ts "||/"
      <|> ts "|/"
      <|> ts "!!"
      <|> ts "@"
      <|> string "~"
       where
        ts = try . symbol sql


joinOps = do
      notFollowedBy (symbol sql "--")
      ts "::"
       <|> ts "||"
       <|> ts "->>"
       <|> ts "#>>"
       <|> ts "#>"
       <|> ts "->"
       <|> ts "+"
       <|> ts "-"
       <|> ts "*"
       <|> ts "/"
       <|> ts "%"
       <|> ts "^"
       <|> ts "|"
       <|> ts "&"
       <|> ts "#"
       <|> ts "<<"
       <|> string ">>"
       where
        ts = try . symbol sql

valueExpression :: WParser ()
valueExpression = do
   qve <- commonValueExpression
--    whiteSpace sql
--    addQ "\n--VE\n"
   addQ qve
   addN $ show qve

valueExpressionWithAsClause  :: WParser ()
valueExpressionWithAsClause  = do
   addQ =<< commonValueExpression
   addQ =<< greservedQ "AS"
   addQN =<< identifierExpr

valueType :: WParser ()
valueType = try (
             do
               void $ string "-- >"
               addC =<< rawHaskell
               addQ1 "\n      "
            ) <|> try (
             do
               void $ string "-- "
               addT =<< rawHaskell
               addQ1 "\n      "
            ) <|> addU

-- <derived column> ::= <value expression> [ <as clause> ]
derivedColumn :: WParser ()
derivedColumn = do
        try  valueExpressionWithAsClause
         <|> valueExpression
        valueType

{-
<value expression> ::=
		<common value expression>
	|	<boolean value expression>
	|	<row value expression>

<common value expression> ::=
		<numeric value expression>
	|	<string value expression>
	|	<datetime value expression>
	|	<interval value expression>
	|	<user-defined type value expression>
	|	<reference value expression>
	|	<collection value expression>
-}

selectList :: WParser ()
selectList = do
        derivedColumn
        optional ((comma sql >>= addQ1) >>  selectList)

-- TODO: check and simplify
--  ******************************

setQuantifier :: WParser ()
setQuantifier = try (greserved "DISTINCT") <|> try (greserved "UNIQUE") <|> greserved "ALL"

parseSelect :: WParser ()
parseSelect = do
             whiteSpace sql
             greserved "SELECT"
             optional setQuantifier
             selectList
             optional $ try typeParams

-- *
optInputSource :: WParser ()
optInputSource = try ( do
         void $ string "-- <"
         addKIS =<< rawHaskell
       ) <|> addKQM
-- *
typedIdentifier3 :: WParser String
typedIdentifier3 = do
    r <- identifier3
    optional $ try ( do 
      void $ string "-- ~"
      addKLit =<< rawHaskell
       )<|> try ( do 
      void $ string "-- >"
      addIC =<< rawHaskellT
      optInputSource
       ) <|> try ( do
         void $ string "-- <"
         addKIS =<< rawHaskell
        ) <|> try ( do 
      void $ string "--"
      addIT =<< rawHaskellT
      optInputSource
       ) <|> (addJU >> addKQM)
    return r

-- *
fields :: WParser String
fields = do
   void . lexeme sql . string $ "("
   ret <- sepBy1 (do whiteSpace sql; try (fieldsDeep 1) <|> typedIdentifier3) (string ",")
   void . lexeme sql . string $ ")"
   return $ DL.intercalate ", " ret

-- *
fieldsDeep :: Int -> WParser String
fieldsDeep depth = do
   deepId <- identifier3
   void . lexeme sql . string $ "("
   ret <- sepBy (do whiteSpace sql; try identifier3 <|> fieldsDeep depth ) (string ",")
   void . lexeme sql . string $ ")"
   when (depth == 1) addKU
   return $ deepId ++ braketS (DL.intercalate "," ret)

-- *
braketS :: String -> String
braketS x = "(" ++ x ++ ")"

quoteS :: String -> String
quoteS  x = "'" ++ x ++ "'"

{- |
This is a simple library which acts as a wrapper for mysql-simple allowing adding Haskell types/functions inside sql queries

TH is heavily in use to generate simple lambda expressions so typechecker can infer types.
Whenever we do not say anything it will have to be infered using some other ways.

If we specify types, the singleton results/inputs does not need to wrapped into Only, the machinery converts it into a parameter
as if 1-tuple == value

All operations happens on the compile time, the generated code is the same as if we have written some (possibly sloppy queries for m-s)



SQL Insert is a bit simplified, we do not specify the VALUES section as it will be generated for us
there are special operatrs:
  we can set fields to
   -- ~ Exactly the text we put after (the exact text is placed in the palace of ? like : VaLues( ...,Exactly,...)
   -- > function fun which will generate ? and the parameter at this location will get the  sql( ...?...)   (fun input)
   -- Type  ->  sql( ...?...) (input :: Type)
   -- < someCalculations   -> sql( ...?...) (someCalculations)

Currently there are two main functions: genJsonQuery,genTypedQuery

which depending on the number of parameters and the type of query will return
a list or (); they will make decision and call one of query, query_, execute or execute_

genJsonQuery will capture the names from AS part of Q and use it to generate Aeson object property of this name and value from the result
genTypedQuery the result of it will be just a list [(r1,..,rn)]

There was some effort made to have IN queries to generate IN param for you; thus in your Q yu put just a list and it should be magically 
wrapped to make m-s

The original m-s allows only a very short ( ;) 10) of query results, there is a TH generator but which has to be run beforehand... I was wondering
if it could be somehow automatised ?

-}

-- TODO: check and simplify
parseInsert :: WParser ()
parseInsert = do
             whiteSpace sql
             greserved "INSERT"
             greserved "INTO"
             addQ =<< identifier3
             addQ =<< braketS <$> fields
             addQ =<< getInput

caseComp :: WParser ()
caseComp = do
  greserved "CASE"
  greserved "WHEN"
  computable
  greserved "THEN"
  computable
  greserved "ELSE"
  computable
  greserved "END"

computable :: WParser ()
computable = do
             try caseComp
               <|> try ( do
                optional $ addQ =<< expandQ (reservedNames sqlDef)
                addQ1 =<< lexeme sql (string "(")
                optional computableC -- with comma
                addQ1 =<< lexeme sql (string ")")
                ) <|> try (addQ =<< identifier1)

             optional $ try (do
                addQ1 =<< ((' ':) <$> arithm)
                computable
                )

computableC :: WParser ()
computableC = do
      computable
      optional $ try $ do
           addQ1 =<< lexeme sql (string ",")
           computableC

{--
SELECT
    [ALL | DISTINCT | DISTINCTROW ]
      [HIGH_PRIORITY]
      [STRAIGHT_JOIN]
      [SQL_SMALL_RESULT] [SQL_BIG_RESULT] [SQL_BUFFER_RESULT]
      [SQL_CACHE | SQL_NO_CACHE] [SQL_CALC_FOUND_ROWS]
    select_expr [, select_expr ...]
    [FROM table_references
    [WHERE where_condition]
    [GROUP BY {col_name | expr | position}
      [ASC | DESC], ... [WITH ROLLUP]]
    [HAVING where_condition]
    [ORDER BY {col_name | expr | position}
      [ASC | DESC], ...]
    [LIMIT {[offset,] row_count | row_count OFFSET offset}]
    [PROCEDURE procedure_name(argument_list)]
    [INTO OUTFILE 'file_name' export_options
      | INTO DUMPFILE 'file_name'
      | INTO var_name [, var_name]]
    [FOR UPDATE | LOCK IN SHARE MODE]]
--} 