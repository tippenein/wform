module Form where

import Data.Lens
import Prelude

import Control.Monad.Aff.Console (log)
import Data.Either (Either(..))
import Data.Lens.Iso (Iso, iso)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import WForm as F

_Newtype :: forall t a s b. Newtype t a => Newtype s b => Iso t s a b
_Newtype = iso unwrap wrap

type State = {
    errors :: Maybe F.FormError
  , registration :: UserReg
  }


data Query a
  = Register (F.FormInput UserReg) a

data Message = Toggled Boolean

myForm :: forall m. H.Component HH.HTML Query Unit Message m
myForm =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = false

  render :: State -> H.ComponentHTML Query
  render state = viewForm state

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval (Register f a) = handleRegister f $> pure a
    where
      handleRegister (F.Edit f) = H.modify (stRegistration %~ f)
      handleRegister (F.Submit) = do
        H.liftAff $ log "asdf"

viewForm st =
  HH.div_ $
    F.renderForm st.registration Register do
      F.textField "name" "User name" (_name) validateNothing
      F.emailField "email" "Email" (_email) validEmail
      F.passwordField "password" "Password" (_password) validPassword
      F.passwordField "confirm" "Confirmation" (_confirmation) validConfirmation
    where
      validPassword str
          | Str.length str < 6 = Left "Password must be at least 6 characters"
          | otherwise = Right str
      validConfirmation str
          | str == st ^. stRegistration <<< _password = Right str
          | otherwise = Left "Password must match confirmation"
      validEmail str = maybe (Left "Must have @ symbol") (const (Right str)) (Str.indexOf (Str.Pattern "@") str)
      validateNothing str = Right str


initialState :: State
initialState = {
    registration: emptyReg
  , errors: Nothing
  }

stRegistration :: Lens' State UserReg
stRegistration =
  lens (_.registration) (_ { registration = _ })

newtype UserReg
  = UserReg
  { email        :: String
  , name         :: String
  , password     :: String
  , confirmation :: String
  }

_UserReg :: Lens' UserReg { email :: String, password :: String, confirmation :: String, name :: String }
_UserReg = _Newtype

derive instance newtypeUserReg :: Newtype UserReg _

_name :: Lens' UserReg String
_name = _Newtype <<< lens (_.name) (_ { name = _ })

_email :: Lens' UserReg String
_email = _UserReg <<< lens (_.email) (_ { email = _ })

_password :: Lens' UserReg String
_password = _Newtype <<< lens (_.password) (_ { password = _ })

_confirmation :: Lens' UserReg String
_confirmation = _Newtype <<< lens (_.confirmation) (_ { confirmation = _ })


emptyReg :: UserReg
emptyReg = mkRegistration "" "" "" ""

stErrors :: Lens' State (Maybe F.FormError)
stErrors = lens _.errors _ { errors = _ }

mkRegistration :: String -> String -> String -> String -> UserReg
mkRegistration e p pc n =
    UserReg { email: e, password: p, confirmation: pc, name: n }

-- derive instance genericUserReg :: Generic UserReg

-- instance showUserReg :: Show UserReg where show = gShow

-- instance eqUserReg :: Eq UserReg where eq = gEq
