module WForm where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, ask, runReaderT)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Either (Either(..))
import Data.Lens (Lens', set, (^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Halogen.HTML (ClassName(..), PropName(..))
import Halogen.HTML as H
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Unsafe.Coerce (unsafeCoerce)

import WForm.Internal (stringToMaybe, styleClass)

type WForm v f a =
  ReaderT
    (Tuple a (FormAction f a))
    (Writer (Array (HTML v (f Unit))))
    a

type FormError = Array String
type FormAction f a = FormInput a -> Unit -> f Unit

data FormInput a
  = Submit
  | Edit (a -> a)

-- | The main function for building a form.
-- Each field has the normal name and label along with a lens for accessing and
-- a validator for the input.
-- ```
-- newtype TheForm = TheForm { email :: String }
--
-- _email = _Newtype <<< prop (SProxy :: SProxy "email")
--
-- renderForm state.form Register do
--   textField "name" "User Name" _email emailValidator
-- ```
renderForm
  :: forall a v f
   . a
  -> FormAction f a
  -> WForm v f a
  -> Array (HTML v (f Unit))
renderForm _data eventType fields =
  execWriter (runReaderT fields (Tuple _data eventType)) <> [submitButton_ (eventType Submit)]

emailField = field P.InputEmail
textField = field P.InputText
passwordField = field P.InputPassword
dateField = field P.InputDate
fileField = field P.InputFile

emailFieldOpt = fieldOpt P.InputEmail
textFieldOpt = fieldOpt P.InputText
passwordFieldOpt = fieldOpt P.InputPassword
dateFieldOpt = fieldOpt P.InputDate
fileFieldOpt = fieldOpt P.InputFile

field
  :: forall a f v
   . P.InputType
  -> String
  -> String
  -> Lens' a String
  -> (String -> Either String String)
  -> WForm v f a
field inpType id_ label lens_ validator = do
    Tuple data_ eventType <- ask
    let item = data_ ^. lens_
        validation = validator item
        Tuple errMsg classes =
          case validation of
            Left str -> Tuple str [ ClassName "form-group has-error" ]
            Right _ -> Tuple "" [ ClassName "form-group" ]
    tell [html data_ eventType errMsg classes item]
    -- TODO: handle this better
    pure (unsafeCoerce item)
  where
    html :: a -> FormAction f a -> String -> Array ClassName -> String -> HTML v (f Unit)
    html data_ eventType errMsg classes item =
      H.div [ P.classes classes ]
        [ H.label [ P.for id_ ] [ H.text label ]
        , H.input
          [ P.id_ id_
          , P.classes [ ClassName "form-control" ]
          , P.prop (PropName "type") inpType
          , P.value item
          , E.onValueChange (E.input (eventType <<< Edit <<< set lens_))
          ]
        , H.span_ [ H.text errMsg ]
        ]
fieldOpt
  :: forall a b f v
   . P.InputType
  -> String
  -> String
  -> Lens' a (Maybe String)
  -> (String -> Either String String)
  -> WForm v f a
fieldOpt inpType id_ label lens_ validator = do
    Tuple data_ eventType <- ask
    let item = data_ ^. lens_
        validation = case item of
                          Nothing -> Right ""
                          Just i -> validator i
        classes = case validation of
                       Left _ -> [ ClassName "form-group has-error" ]
                       Right _ -> [ ClassName "form-group" ]
        errMsg = case validation of
                      Left str -> str
                      Right _ -> ""
    tell [html eventType errMsg classes item]
    -- TODO: handle this better
    pure (unsafeCoerce item)
  where
    html :: FormAction f a -> String -> Array ClassName -> (Maybe String) -> HTML v (f Unit)
    html eventType errMsg classes item =
      H.div [ P.classes classes ]
        [ H.label [ P.for id_ ] [ H.text label ]
        , H.input
          [ P.id_ id_
          , P.classes [ ClassName "form-control" ]
          , P.prop (PropName "type") inpType
          , P.value $ fromMaybe "" item
          , E.onValueChange (E.input (\a -> eventType (Edit (\b -> set lens_ (stringToMaybe a) b))))
          ]
        , H.span_ [ H.text errMsg ]
        ]

submitButton t h =
  H.button
  -- TODO fix no propogate and preventDefault
    [ E.onClick (E.input_ h)
    , styleClass "btn btn-primary"
    , P.type_ P.ButtonSubmit ]
    [ H.text t ]

submitButton_ = submitButton "Submit"
