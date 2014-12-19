typedquery
==========

Parser for SQL augmented with types

This package provides base parsing facilities for possibly all *-simple database packages converting them into *-simpel-typed

The basic idea is to start using SQL again, but use comemnts (`--`) to hide haskell annotation.

This started as `QuasiQuotes` excercise with the TH inpired `printf`.

 * `genJsonQuery` produces `[Value]`
 * `genTypedQuery` produces `[(T1,...,Tn)]` tuples, `[T]` or `()` all depending on the `SQL` query

If you do not provide value (or a mean to get on inside query you need to give it outside.


They do the same:

    $(genJsonQuery "SET SESSION group_concat_max_len = ? ") conn (10000 :: Int)
    $(genJsonQuery "SET SESSION group_concat_max_len = ? -- Int ") conn 10000
    $(genJsonQuery "SET SESSION group_concat_max_len = ? -- Int  -- < 1000 ") conn
    $(genJsonQuery "SET SESSION group_concat_max_len = ? -- < (1000 :: Int) ") conn


There is a basic syntax, and the base idea is to have a nice easy for eye syntax.
It fires the correct `execute` or `query` with or without `_` depending on the actual `SQL` syntax

The parser is not complete, I will try to add as many issues there are and try to fix it.

Adnotations start with `-- ` as otherwise `HeidiSQL` was complaining, then  `>` `<` `~`  or just text.

------------------------- | ------------------------------
| syntax                  |     equivalent               |
------------------------- | ------------------------------
|    bal -- Type          |  (\v -> v :: Bla)            |
|    bla -- > f           |  (\v -> f bla )              |
|    bla -- Type -- > f   |  (\v -> (f bla):: Type )     |
------------------------- | ------------------------------
|    ?   -- Type -- < var |  ??                          |
|    ?   -- < var         |  ??                          |
|    ?   -- < var         |  ??                          |
|    ?   -- ~ verbatim    |  ??                          |
------------------------- | ------------------------------


Eg.

    insert into some_table
      ( timeAsSQLfunction           -- ~ now ()
      , someInputfromAesonViaLens   -- Int  -- < v ^? (key "coolValue" . _Integral) ^. non 3 
      , someUserName                -- Text -- < someNameFromContext
      )

Translates to

    execute conn [qq| insert into some_table
          ( timeAsSQLfunction, someInputfromAesonViaLens, someUserName )
          values ( now (), ?, ?) |] 
            [( (v ^? (key "coolValue" . _Integral) ^. non 3 ) :: Int, someNameFromContext Text)]
