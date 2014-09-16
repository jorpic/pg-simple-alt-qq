
module Database.PostgreSQL.Simple.SqlQQ.Alt
  (sql, sqlQuoter
  ) where

import Data.List.Split (splitOn)
import Data.Either (partitionEithers)

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.Meta (parseExp)


sql :: QuasiQuoter
sql = sqlQuoter "$(" ")$"

sqlQuoter :: String -> String -> QuasiQuoter
sqlQuoter from to = QuasiQuoter
  {quotePat  = error ""
  ,quoteType = error ""
  ,quoteDec  = error ""
  ,quoteExp  = \s -> case parseQuery from to s of
    Right (query, [param]) -> tupE [stringE query, listE [param]]
    Right (query, params)  -> tupE [stringE query, hlistE params]
    Left err -> error err
  }

hlistE :: [ExpQ] -> ExpQ
hlistE = \case
  []   -> tupE []
  [x]  -> x
  x:xs ->
    let cons = conE $ mkName ":."
    in cons `appE` x `appE` hlistE xs


parseQuery :: String -> String -> String -> Either String (String, [ExpQ])
parseQuery from to str
  = case splitOn from str of
    [] -> Right ("", [])
    prefix : rest ->
      let parts = map (splitOn to) rest
          exprs = map head parts
          tails = map tail parts
          query = prefix ++ concatMap (concat . ("?":)) tails
      in case partitionEithers $ map parseExp exprs of
        ([], hsExprs) -> Right (query, map return hsExprs)
        (err:_, _)    -> Left err
