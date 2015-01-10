[![Coverage Status](https://img.shields.io/coveralls/tolysz/typedquery.svg)](https://coveralls.io/r/tolysz/typedquery)
[![Build Status](https://travis-ci.org/tolysz/typedquery.svg?branch=master)](https://travis-ci.org/tolysz/typedquery)
[![Latest Version](https://img.shields.io/hackage/v/typedquery.svg)](https://hackage.haskell.org/package/typedquery)


typedquery
==========

Parser for SQL augmented with types

Till it finalised I would recomend installing it: with `cabal -f debug-typed-queries` just to see what is gonig on. 
 As the SQL parser is not complete, and will need a major lift to make it readable and sane... however it works fine for all queries I need.

This package provides base parsing facilities for possibly all *-simple database packages converting them into *-simpel-typed

 * https://github.com/tolysz/mysql-simple-typed
 * https://github.com/tolysz/sqlite-simple-typed
 * https://github.com/tolysz/postgresql-simple-typed

example: https://github.com/tolysz/sqlite-simple-typed/blob/master/example/src/Main.hs

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

    
| syntax                  |     equivalent               |
------------------------- | ------------------------------
|    `bal -- Type`          |  `(\v -> v :: Bla)`            |
|    `bla -- > f`           |  `(\v -> f bla )`              |
|    `bla -- Type -- > f`   |  `(\v -> (f bla):: Type )`     |
|    `?   -- Type -- < var` |  `??`                          |
|    `?   -- < var`         |  `??`                          |
|    `?   -- < var`         |  `??`                          |
|    `?   -- ~ verbatim`    |  `??`                          |
 


Eg.

    $(genJsonQuery [qq| insert into some_table
      ( timeAsSQLfunction           -- ~ now ()
      , someInputfromAesonViaLens   -- Int  -- < v ^? (key "coolValue" . _Integral) ^. non 3 
      , someUserName                -- Text -- < someNameFromContext
      ) |]) conn

Translates to

    execute conn [qq| insert into some_table
          ( timeAsSQLfunction, someInputfromAesonViaLens, someUserName )
          values ( now (), ?, ?) |] 
            [( (v ^? (key "coolValue" . _Integral) ^. non 3 ) :: Int, someNameFromContext Text)]
