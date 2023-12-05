{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.Aeson
import Data.Text (Text, null)

data Effect = Allow | Deny
  deriving (Show, Eq)

newtype Action = Action Text
  deriving (Show, Eq)

newtype Resource = Resource Text
  deriving (Show, Eq)

data Statement = Statement
  { effect :: Effect,
    action :: Action,
    resource :: Resource
  }
  deriving (Show, Eq)

data Policy = Policy
  { version :: Text,
    statements :: [Statement]
  }
  deriving (Show, Eq)

instance FromJSON Effect where
  parseJSON = withText "Effect" $ \t ->
    case t of
      "Allow" -> return Allow
      "Deny" -> return Deny
      _ -> fail "Invalid effect"

instance FromJSON Action where
  parseJSON = withText "Action" $ \t -> return $ Action t

instance FromJSON Resource where
  parseJSON = withText "Resource" $ \t -> return $ Resource t

instance FromJSON Statement where
  parseJSON = withObject "Statement" $ \obj -> do
    eff <- obj .: "Effect"
    act <- obj .: "Action"
    res <- obj .: "Resource"
    return $ Statement eff act res

instance FromJSON Policy where
  parseJSON = withObject "Policy" $ \obj -> do
    ver <- obj .: "Version"
    sts <- obj .: "Statement"
    return $ Policy ver sts

validateStatement :: Statement -> Bool
validateStatement stmt =
  case (effect stmt, action stmt, resource stmt) of
    (Allow, Action a, Resource r) -> not (Data.Text.null a) && not (Data.Text.null r)
    (Deny, Action a, Resource r) -> not (Data.Text.null a) && not (Data.Text.null r)

validatePolicy :: Policy -> Bool
validatePolicy (Policy version stmts) = all validateStatement stmts && not (Data.Text.null version)

explicitDeny :: Policy -> Bool
explicitDeny (Policy _ stmts) =
  any
    ( \stmt ->
        case (effect stmt, action stmt, resource stmt) of
          (Deny, Action a, Resource r) -> a == a && r == r
          _ -> False
    )
    stmts

explicitAllow :: Policy -> Bool
explicitAllow (Policy _ stmts) =
  any
    ( \stmt ->
        case (effect stmt, action stmt, resource stmt) of
          (Allow, Action a, Resource r) -> a == a && r == r
          _ -> False
    )
    stmts

hasPermission :: Policy -> Action -> Resource -> Bool
hasPermission (Policy version stmts) (Action a) (Resource r) = res
  where
    deny = explicitDeny (Policy version stmts)
    allow = explicitAllow (Policy version stmts)
    res = not deny && allow

main :: IO ()
main = do
  let json = "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Action\":\"s3:ListBucket\",\"Resource\":\"arn:aws:s3:::examplebucket\"},{\"Effect\":\"Allow\",\"Action\":\"s3:GetObject\",\"Resource\":\"arn:aws:s3:::examplebucket/*\"}]}"
  let policy = decode json :: Maybe Policy
  print $ maybe False validatePolicy policy
