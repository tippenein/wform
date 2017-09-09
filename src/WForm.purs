module WForm where

import Control.Monad.Reader.Trans (ReaderT, ask, runReaderT)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Lens (Lens', set, (^.))
import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML (ClassName(..))
import Halogen.HTML as H
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

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
          -- , P.inputType inpType
          , P.value item
          , E.onValueChange (E.input (eventType <<< Edit <<< set lens_))
          ]
        , H.span_ [ H.text errMsg ]
        ]

styleClass = P.class_ <<< H.ClassName

submitButton t h =
  H.button
  -- TODO fix no propogate and preventDefault
    [ E.onClick (E.input_ h)
    , styleClass "btn btn-primary"
    , P.type_ P.ButtonSubmit ]
    [ H.text t ]

submitButton_ = submitButton "Submit"
