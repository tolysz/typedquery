typedquery
==========

Parser for SQL augmented with types

This package provides base parsing facilities for possibly all *-simple database packages converting them into *-simpel-typed

The basic idea is to start using SQL again, but use comemnts (`--`) to hide haskell annotation.

This started as `QuasiQuotes` excercise with the TH inpired `printf`.

`genJsonQuery` produces `[Value]`

They do the same: 

    $(genJsonQuery "SET SESSION group_concat_max_len = ? -- Int ") conn 10000
    $(genJsonQuery "SET SESSION group_concat_max_len = ? -- Int  -- < 1000 ") conn
    $(genJsonQuery "SET SESSION group_concat_max_len = ? -- < (1000 :: Int) ") conn
