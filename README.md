
This bit of code allows to put Haskell expressions inside SQL queries.

    {-# LANGUAGE QuasiQuotes #-}
    import Data.Time.Clock (getCurrentTime)
    import Database.PostgreSQL.Simple
    import Database.PostgreSQL.Simple.SqlQQ.Alt (sql)

    testQuery c = do
      let dbName = "test_db"
      now <- getCurrentTime
      uncurry (query c)
        [sql|
          select * from pg_stat_activity
          where query_start < $(addUTCTime (-60) now)$
            and datname = $(dbName)$
        |]

Don't forget to enable `OverloadedStrings` extension, owherwise you can encounter errors like:

    Couldn't match expected type ‘Query’ with actual type ‘[Char]’
    
== TODO
 - SQL lexer
