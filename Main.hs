{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Applicative
import qualified Control.Monad
import Data.Maybe (fromMaybe)
import Parser

data Effect = Allow | Deny deriving (Show, Eq)

stringToEffect :: String -> Maybe Effect
stringToEffect "Allow" = Just Allow
stringToEffect "Deny" = Just Deny
stringToEffect _ = Nothing

data Statement = Statement
  { effect :: Effect,
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
    (Allow, a, r) -> not (null a) && not (null r)
    (Deny, a, r) -> not (null a) && not (null r)

validatePolicy :: Maybe Policy -> Bool
validatePolicy (Just p) = all validateStatement stmts && not (null version)
  where
    stmts = statements p
    version = Main.version p
validatePolicy Nothing = False

explicitDeny :: Policy -> String -> String -> Bool
explicitDeny (Policy _ stmts) a r =
  any
    ( \stmt ->
        case (effect stmt, action stmt, resource stmt) of
          (Deny, a', r') | a == a' && r == r' -> True
          _ -> False
    )
    stmts

explicitAllow :: Policy -> String -> String -> Bool
explicitAllow (Policy _ stmts) a r =
  any
    ( \stmt ->
        case (effect stmt, action stmt, resource stmt) of
          (Allow, a', r') | a == a' && r == r' -> True
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
  let Just (JsonString va) = extractValueFromKeyValPairs "Effect" xs
      Just (JsonString vb) = extractValueFromKeyValPairs "Action" xs
      Just (JsonString vc) = extractValueFromKeyValPairs "Resource" xs
      Just va' = stringToEffect va
   in case (va', vb, vc) of
        (a, b, c) -> Just (Statement a b c)

arrangePolicy :: JsonValue -> Maybe Policy
arrangePolicy (JsonObject xs) = do
  let versionJsonValue = fromMaybe (JsonString "") (extractValueFromKeyValPairs "Version" xs)
  let (JsonArray stmts) = fromMaybe (JsonArray []) (extractValueFromKeyValPairs "Statement" xs)

  let stmts' = [stmt | JsonObject stmt <- stmts]
  let stmts'' = [arrangeStatement stmt | stmt <- stmts']
  let stmts''' = sequenceA stmts''

  case stmts''' of
    Just stmts'''' -> arrangePolicy' versionJsonValue (Just stmts'''')
    Nothing -> Nothing
  where
    arrangePolicy' :: JsonValue -> Maybe [Statement] -> Maybe Policy
    arrangePolicy' (JsonString version) (Just stmts) = Just $ Policy version stmts
    arrangePolicy' _ _ = Nothing
arrangePolicy _ = Nothing

main :: IO ()
main = do
  let json = "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Action\":\"s3:ListBucket\",\"Resource\":\"arn:aws:s3:::examplebucket\"},{\"Effect\":\"Deny\",\"Action\":\"s3:ListBucket\",\"Resource\":\"arn:aws:s3:::examplebucket2\"}]}"
  let Just (_, parsed) = runParser jsonValue json

  let policy = arrangePolicy parsed
  let isValid = validatePolicy policy

  print policy

  if isValid
    then do
      let has1 = hasPermission policy "s3:ListBucket" "arn:aws:s3:::examplebucket"
      let has2 = hasPermission policy "s3:ListBucket" "arn:aws:s3:::examplebucket2"
      print "Policy is valid"
      print $ "Has permission for s3:ListBucket on arn:aws:s3:::examplebucket: " ++ show has1
      print $ "Has permission for s3:ListBucket on arn:aws:s3:::examplebucket2: " ++ show has2
    else do
      print "Policy is not valid"
