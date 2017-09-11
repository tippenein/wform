module WForm.Internal where

import Data.Maybe (Maybe(..))
import Halogen.HTML as H
import Halogen.HTML.Properties as P
import Prelude ((<<<))

stringToMaybe :: String -> Maybe String
stringToMaybe "" = Nothing
stringToMaybe a = Just a

styleClass = P.class_ <<< H.ClassName
