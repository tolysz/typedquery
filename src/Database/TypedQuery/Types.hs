{-# Language DeriveDataTypeable
  , RankNTypes
  , CPP
  , TemplateHaskell
  , LambdaCase
  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Database.TypedQuery.Types
  ( TypedQuery (..)
  , TypeAction (..)
  , RunDB (..)
  , jsonPair
  , convAction
  , typeTuple
  , typeTupleIn
  , vanilaQuery
  , genUncurry
  , genJsonQuery
  , genTypedQuery
  )
where

import Control.Arrow (first)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Typeable (Typeable)

import Data.Aeson (object)
import Data.Aeson.Types (ToJSON(..),Value)

import Database.TypedQuery.SQLParser
import Data.Text (Text,unpack)
import Prelude
import Control.Monad
import Control.Applicative
import Data.Either
import Data.Maybe

import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

import Language.Haskell.Meta.Parse (parseType, parseExp)


#if DEVELOPMENT
import Language.Haskell.Exts (parseStmtWithMode, prettyPrint, ParseResult(..), defaultParseMode, Extension(..),KnownExtension(..), ParseMode(..))
-- import Language.Haskell.Exts (parseStmt, prettyPrint, ParseResult(..), defaultParseMode, Extension(..),KnownExtension(..), ParseMode(..))
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)

import Data.String.QM

codeFormat = id
-- currently there is some parese error in the following section
codeFormat' =  check . fmap reformat . parseStmtWithMode defaultParseMode {
  extensions
    = [ EnableExtension BangPatterns
      , EnableExtension HereDocuments
      , EnableExtension OverloadedStrings
      , EnableExtension NewQualifiedOperators
      , EnableExtension TypeOperators
      ]}
 where
  reformat = prettyPrint
  check = \case
      ParseOk a ->  a
      ParseFailed loc err -> error $ show (loc,err)

#endif

class RunDB q where
  rdquery    :: q -> TH.Name
  rdquery_   :: q -> TH.Name
  rdexecute  :: q -> TH.Name
  rdexecute_ :: q -> TH.Name
  rdin       :: q -> TH.Name
  rdonly     :: q -> TH.Name
  rdconn     :: q -> TH.Name

data RunDB q => TypedQuery q
-- ^ `TypedQuery` is a represent SQL query + all fields we could parse from it
--  the idea is to use the original SQL and use comments `--` to annotate it with some extra information
  = TypedQuery
   { fromTypedQuery        ::  q -- SQL text of the query
   , namesTypedQuery       :: [Text]        -- names from fields
   , typesTypedQuery       :: [TypeAction]  -- types of fields
   , typesTypedInput       :: [TypeAction]  -- types of "?" inputs
   , typesTypedInputSource :: [TypeAction]  -- functions feeding data
   }
  deriving (Eq, Ord, Typeable)

flat :: RunDB q => TypedQuery q -> (q, [Text], [TypeAction], [TypeAction], [TypeAction])
flat (TypedQuery a b c d e) = (a, b, c, d, e)

instance (RunDB q, TH.Lift q) => TH.Lift (TypedQuery q) where
  lift = TH.lift . flat

instance TH.Lift Text where
  lift = TH.lift . unpack

instance (Show q,RunDB q) => Show (TypedQuery q) where
    show = show . fromTypedQuery

instance (RunDB q, IsString q, Monoid q) => Read (TypedQuery q) where
    readsPrec i = fmap (first conv) . readsPrec i
      where
        conv x = case typedSQLtoSQL x of
          Nothing -> mempty
          Just (a, b, c, d, e) -> TypedQuery (fromString a) b c d e

instance (RunDB q, IsString q, Monoid q) => IsString (TypedQuery q) where
    fromString x =
     case typedSQLtoSQL x of
       Nothing -> mempty
       Just (a, b, c, d, e)    -> TypedQuery (fromString a) b c d e

instance (RunDB q, Monoid q) => Monoid (TypedQuery q) where
    mempty = TypedQuery mempty mempty mempty mempty mempty
    TypedQuery a1 a2 a3 a4 a5 `mappend` TypedQuery b1 b2 b3 b4 b5 
        = TypedQuery
            (mappend a1 b1)
            (mappend a2 b2)
            (mappend a3 b3)
            (mappend a4 b4)
            (mappend a5 b5)
    {-# INLINE mappend #-}

convAction :: RunDB q => q -> TypeAction -> TH.Exp -> TH.Exp
convAction _ (TAUnknown) = id
convAction _ (TAConv s) = case parseExp s of
    Right e -> TH.AppE e
    Left er -> error $ "Could not find `convExp` type: " ++ er

convAction  _ (TACast s) = case parseType s of
    Right t1 -> (`TH.SigE` t1)
    Left er -> error $ "Could not find `typeExp` type: " ++ er

convAction q (TAInUnknown) = TH.AppE (TH.ConE (rdonly q))
convAction q (TAInConv s) = case parseExp s of
    Right ex -> TH.AppE (TH.ConE (rdin q)) . TH.AppE ex
    Left er -> error $ "Could not find `convExp` type: " ++ er
convAction q (TAInCast s) = case parseType s of
    Right t1 -> \ e -> TH.ConE (rdin q) `TH.AppE` (e `TH.SigE` t1)
    Left er -> error $ "Could not find `typeExp` type: " ++ er


jsonPair :: RunDB q => TypedQuery q -> TH.Q TH.Exp
jsonPair (TypedQuery q li lq _ _) = do
       let k = length li
       nam <- mapM TH.lift li
       nsa <- replicateM k (TH.newName "a")
       let typ = zipWith (\a b -> convAction q a (TH.VarE b)) lq nsa
       case k of
        1 -> do
            let n1 = head nam
            let t1 = head typ
            return $ TH.LamE
                 [TH.BangP $ TH.ConP (rdonly q) [ TH.VarP $ head nsa] ]
                 ( TH.AppE (TH.VarE 'object) (TH.ListE [TH.TupE [n1, TH.AppE (TH.VarE 'toJSON) t1]])) 
        _ -> return $ TH.LamE
                 [TH.TupP $ map (TH.BangP . TH.VarP) nsa]
                 ( TH.AppE (TH.VarE 'object) (TH.ListE $ zipWith (\a b-> TH.TupE [ a, TH.AppE (TH.VarE 'toJSON) b `TH.SigE` TH.ConT ''Value]) nam typ ) )

typeTuple :: RunDB q => TypedQuery q -> TH.Q TH.Exp
typeTuple (TypedQuery q _ lq _ _) = do
       let k = length lq
       nsa <- replicateM k (TH.newName "a")
       let typ = zipWith (\a b -> convAction q a (TH.VarE b)) lq nsa
       case k of
        1 -> return $ TH.LamE [TH.BangP $ TH.ConP (rdonly q) [ TH.VarP $ head nsa] ] (head typ)
        _ -> return $ TH.LamE [TH.TupP  $ map (TH.BangP . TH.VarP) nsa         ] (TH.TupE typ)

genVal :: RunDB q => q ->  TypeAction -> Either a (Maybe TH.Exp)
genVal _ (TAInsIS a)    = Right $ either (error "parse failed") Just (parseExp a)
genVal _ (TAInsISU a)   = Right $ either (error "parse failed") Just (parseExp a)
genVal q (TAInSupply a) = Right $ either (error "parse failed") (Just . TH.AppE (TH.ConE (rdin q)) ) (parseExp a)
genVal _ _ = Right Nothing


handleOnly :: RunDB q => q -> [TH.Exp] -> TH.Exp
handleOnly q [a] = TH.ConE (rdonly q) `TH.AppE` a
handleOnly _ a = TH.TupE a

typeTupleIn :: RunDB q => TypedQuery q -> TH.Q(Bool,Bool,TH.Exp)
typeTupleIn (TypedQuery q _ _ lq lextra) = do
       nsa <- replicateM (length lq) (TH.newName "i")

       let initTyp = zipWith (\a b -> if b == TAInsQM || b == TAInsQMU then Left a else genVal q b  )  nsa lextra
           typ = zipWith (\a c -> case c of
                   Left v -> Just $ convAction q a (TH.VarE v)
                   Right v -> convAction q a <$> v
              ) lq initTyp
           pat1 = lefts initTyp
           reduce_params b = if b then id else TH.LamE [TH.TupP $ map (TH.BangP . TH.VarP) pat1]

       return (null pat1, null typ , reduce_params (null pat1)  (handleOnly q $ catMaybes typ))


genUncurry :: RunDB q => q -> Int -> TH.Q TH.Exp
genUncurry _ 0 = do
     f <- TH.newName "f"
     return (TH.VarE f)
genUncurry q 1 = do
     f <- TH.newName "f"
     x <- TH.newName "x"
     return $
       TH.LamE [TH.VarP f, rdonly q `TH.ConP` [TH.VarP x]] $ TH.VarE f `TH.AppE` TH.VarE x
genUncurry _ n
    | n < 0     = fail "There are no negative count tuples"
    | otherwise = do
       f  <- TH.newName "f"
       xs <- replicateM n (TH.newName "x")
       return $
          TH.LamE [TH.VarP f, TH.TupP $ map (TH.BangP . TH.VarP) xs] $ 
             foldl1 TH.AppE (map TH.VarE (f : xs))


vanilaQuery :: (RunDB q, TH.Lift q) => (TypedQuery q -> TH.Q TH.Exp) -> TypedQuery q -> TH.Q TH.Exp
-- ^ general query which can be parametrized depending on the needs
--   it takes a transformation function and some typed Query
vanilaQuery  tran tq@(TypedQuery q _ lq _ _)  = do
             conn <- TH.newName "conn"
             qp   <- TH.newName "qp"
             tmp1 <- TH.newName "res1"
             q1 <- [| q |]
             pa <- tran tq
             (null_out, null_int, initial) <- typeTupleIn tq
             let final_type_cast b = if b then id else TH.AppE (TH.AppE (TH.VarE 'fmap) (TH.VarE 'map `TH.AppE` pa))
                 type_connection x =  TH.VarE x `TH.AppE` (TH.VarE conn `TH.SigE` TH.ConT (rdconn q)) `TH.AppE` q1
                 has_no_output = null lq
                 (qqqq, pat) =
                    case (has_no_output, null_int , null_out) of
                     (True  , True , _ )    -> ( type_connection (rdexecute_ q), [])
                     (True  , False, False) -> ( type_connection (rdexecute  q) `TH.AppE` TH.AppE initial (TH.VarE qp) , [TH.VarP qp] )
                     (True  , False, True ) -> ( type_connection (rdexecute  q) `TH.AppE` initial                  , []        )
                     (False , True , _ )    -> ( type_connection (rdquery_   q), [])
                     (False , False, False) -> ( type_connection (rdquery    q) `TH.AppE` TH.AppE initial (TH.VarE qp) , [TH.VarP qp] )
                     (False , False, True ) -> ( type_connection (rdquery    q) `TH.AppE` initial                  , []        )
                 exp = TH.LamE (TH.VarP conn : pat) $ final_type_cast has_no_output qqqq 
#if DEVELOPMENT
             unsafePerformIO $ do
                putStrLn $ codeFormat  $ TH.pprint exp
                liftIO $ return $ return $ exp
#else
             return exp
#endif

genJsonQuery :: (RunDB q, TH.Lift q) => TypedQuery q -> TH.Q TH.Exp
genJsonQuery = vanilaQuery jsonPair

genTypedQuery :: (RunDB q, TH.Lift q) => TypedQuery q -> TH.Q TH.Exp
genTypedQuery = vanilaQuery typeTuple


{--
class SimpleQuery q where
  query :: (ToRow q, FromRow r) => Connection -> Query -> q -> IO [r]
  query_ :: FromRow r => Connection -> Query -> IO [r]
  -- queryNamed :: FromRow r => Connection -> Query -> [NamedParam] -> IO [r]queryNamed :: FromRow r => Connection -> Query -> [NamedParam] -> IO [r]
  execute :: ToRow q => Connection -> Query -> q -> IO ()
  execute_ :: Connection -> Query -> IO ()

--}



