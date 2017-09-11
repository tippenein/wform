module WForm.Validation where

import Prelude
import Data.Either
import Data.Maybe
import Data.String as Str
import Text.Email.Validate (EmailAddress(..), emailAddress)

optional :: String -> Either String String
optional a = Right a

nonBlank :: String -> Either String String
nonBlank a = case Str.length a of
                0 -> Left "can't be blank"
                _ -> Right a

emailValidator :: String -> Either String String
emailValidator "" = Right ""
emailValidator email = case emailAddress email of
      Nothing -> Left "invalid email"
      Just e -> Right email
