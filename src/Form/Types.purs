module Form.Types where

import Prelude

import DOM.Classy.Event (preventDefault)
import DOM.Classy.Event as E
import Data.Maybe (Maybe(..))
import Halogen (action)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

styleClass = P.class_ <<< H.ClassName

data FormInput a
  = Submit
  | Edit (a -> a)

submitButton t h =
  H.button
  -- TODO fix no propogate and preventDefault
    [ E.onClick (\_ -> action h)
    , styleClass "btn btn-primary"
    , P.type_ P.ButtonSubmit ]
    [ H.text t ]

submitButton_ = submitButton "Submit"
