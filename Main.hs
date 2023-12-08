{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Applicative
import Data.Maybe (fromMaybe)
import Parser

data Statement = Statement
  { effect :: String,
    action :: String,
    resource :: String
  }
  deriving (Show, Eq)

data Policy = Policy
  { version :: String,
    statements :: [Statement]
  }
  deriving (Show, Eq)

validateStatement :: Statement -> Bool
validateStatement stmt =
  case (effect stmt, action stmt, resource stmt) of
    (e, a, r) -> not (null e) && not (null a) && not (null r)

validatePolicy :: Policy -> Bool
validatePolicy (Policy version stmts) = all validateStatement stmts && not (null version)

explicitDeny :: Policy -> String -> String -> Bool
explicitDeny (Policy _ stmts) a r =
  any
    ( \stmt ->
        case (effect stmt, action stmt, resource stmt) of
          ("Deny", a', r') | a == a' && r == r' -> True
          _ -> False
    )
    stmts

explicitAllow :: Policy -> String -> String -> Bool
explicitAllow (Policy _ stmts) a r =
  any
    ( \stmt ->
        case (effect stmt, action stmt, resource stmt) of
          ("Allow", a', r') | a == a' && r == r' -> True
          _ -> False
    )
    stmts

hasPermission :: Maybe Policy -> String -> String -> Bool
hasPermission Nothing _ _ = False
hasPermission (Just policy) a r = not (explicitDeny policy a r) && explicitAllow policy a r

extractValueFromKeyValPairs :: String -> [(String, JsonValue)] -> Maybe JsonValue
extractValueFromKeyValPairs key [] = Nothing
extractValueFromKeyValPairs key ((k, v) : xs) =
  if k == key
    then Just v
    else extractValueFromKeyValPairs key xs

arrangeStatement :: [(String, JsonValue)] -> Maybe Statement
arrangeStatement xs =
  let va = extractValueFromKeyValPairs "Effect" xs
      vb = extractValueFromKeyValPairs "Action" xs
      vc = extractValueFromKeyValPairs "Resource" xs
   in case (va, vb, vc) of
        (Just (JsonString a), Just (JsonString b), Just (JsonString c)) -> Just (Statement a b c)
        _ -> Nothing

arrangePolicy :: String -> [JsonValue] -> Maybe Policy
arrangePolicy version stmts = do
  let stmts' = [stmt | JsonObject stmt <- stmts]
  let stmts'' = [arrangeStatement stmt | stmt <- stmts']
  let stmts''' = sequenceA stmts''

  case stmts''' of
    Just stmts'''' -> Just (Policy version stmts'''')
    Nothing -> Nothing

main :: IO ()
main = do
  let json = "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Action\":\"s3:ListBucket\",\"Resource\":\"arn:aws:s3:::examplebucket\"},{\"Effect\":\"Deny\",\"Action\":\"s3:ListBucket\",\"Resource\":\"arn:aws:s3:::examplebucket2\"}]}"

  let Just (whatsLeft, JsonObject xs) = runParser jsonValue json

  let [(_, JsonString version)] = [(name, value) | (name, value) <- xs, name == "Version"]
  let [(_, JsonArray stmts)] = [(name, value) | (name, value) <- xs, name == "Statement"]

  let policy = arrangePolicy version stmts
  let has1 = hasPermission policy "s3:ListBucket" "arn:aws:s3:::examplebucket"
  let has2 = hasPermission policy "s3:ListBucket" "arn:aws:s3:::examplebucket2"

  print policy
  print has1
  print has2