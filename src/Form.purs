module Form where

import Data.Lens (Lens', set, (^.))
import Prelude

import Data.Either (Either(Right, Left))
import Form.Types (FormInput(Edit, Submit), submitButton_)
import Halogen.HTML (ClassName(ClassName), HTML)
import Halogen.HTML as H
import Halogen.HTML.Events (input, onValueChange) as E
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties as P


lensyForm
  :: forall a b c d e. a
  -> (FormInput e -> b)
  -> Array (a -> (FormInput e -> b) -> HTML c d)
  -> HTML c d
lensyForm data_ eventType fields =
  form (eventType Submit) (map (\f -> f data_ eventType) fields)

-- | A base for building validated form fields
-- lensyPassword = lensyValidatingField P.InputPassword
--
-- lensyEmail
--   :: forall a f v
--    . String
--   -> String
--   -> Lens' a String
--   -> a
--   -> (FormInput a -> Unit -> f Unit)
--   -> HTML v (f Unit)
-- lensyEmail id_ label lens_ = lensyValidatingField P.InputEmail id_ label lens_ validEmail
--   where
--     validEmail str = maybe (Left "Must have @ symbol") (const (Right str)) (Str.indexOf "@" str)
lensyValidatingField
  :: forall a f v.
     P.InputType
  -> String
  -> String
  -> Lens' a String
  -> (String -> Either String String)
  -> a
  -> (FormInput a -> Unit -> f Unit)
  -> HTML v (f Unit)
lensyValidatingField type_ id_ label lens_ validator data_ eventType =
  H.div_
    [ H.label [ P.for id_ ] [ H.text label ]
    , H.input
      [ P.id_ id_
      -- , ClassName "form-control"
      -- , P.inputType type_
      , P.value item
      , E.onValueChange (E.input (eventType <<< Edit <<< set lens_))
      ]
    , H.span_ [ H.text errMsg ]
    ]
  where
    validation = validator item
    item = data_ ^. lens_
    classes = case validation of
                   Left _ ->  [ ClassName "form-group has-error" ]
                   Right _ -> [ ClassName "form-group" ]
    errMsg = case validation of
                  Left str -> str
                  Right _ -> ""

lensyField
  :: forall a f v.
     P.InputType
  -> String
  -> String
  -> Lens' a String
  -> a
  -> (FormInput a -> Unit -> f Unit)
  -> HTML v (f Unit)
lensyField type_ id_ label lens_ data_ eventType = lensyValidatingField type_ id_ label lens_ Right data_ eventType

form onSubmit fields =
  H.form_
    [ H.div [ styleClass "form-group" ]
      (submitButton_ onSubmit <> fields)
    ]

styleClass = HP.class_ <<< ClassName

text = field P.InputText

email = field P.InputEmail

date = field P.InputDate

password = field P.InputPassword

field type_ id_ label value onChange =
  H.div [ P.classes [ ClassName "form-group" ] ]
    [ H.label [ P.for id_ ] [ H.text label ]
    , H.input
      [ P.id_ id_
      , styleClass "form-control"
      , P.type_ type_
      , P.value value
      , E.onValueChange (E.input onChange)
      ]
    ]
