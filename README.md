WForm
----

## What this is

- a spin-off of @parsonsmatt / ql-purs implementation of [WForm](https://github.com/parsonsmatt/ql-purs/blob/master/src/Form/WForm.purs)
- specific to halogen
- specific to bootstrap form styles

## What it should be

- a generic forms library for purescript
  * not specific to halogen
  * not specific to any css style framework

## example usage

**TODO** This isn't up to date and should probably be entirely moved
```purescript
renderView Registration st =
    H.div_ $ errs st.errors :
        WF.renderForm st.registration Register do
            WF.textField "name" "User name:" (_UserReg <<< name) urlSafe
            WF.emailField "email" "Email:" (_UserReg <<< email) validEmail
            WF.passwordField "password" "Password:" (_UserReg <<< password) validPassword
            WF.passwordField "confirm" "Confirmation:" (_UserReg <<< confirmation) validConfirmation
    where
      validPassword str
          | Str.length str < 6 = Left "Password must be at least 6 characters"
          | otherwise = Right str
      validConfirmation str
          | str == st ^. stRegistration <<< _UserReg <<< password = Right str
          | otherwise = Left "Password must match confirmation"
      validEmail str = maybe (Left "Must have @ symbol") (const (Right str)) (Str.indexOf "@" str)
      urlSafe str = case Reg.match (Reg.regex "^[\\w\\d-_]*$" Reg.noFlags) str of
                         Just _ -> Right str
                         Nothing -> Left "Only alphanumeric characters, '_', and '-' are allowed."
```

**TODO** add a runnable example to this repo

Full example [here](https://github.com/parsonsmatt/ql-purs/blob/master/src/QuickLift/View.purs#L80-L109)
