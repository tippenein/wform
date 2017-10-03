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

```purescript
renderView Registration st =
    H.div_ $ errs st.errors :
      -- The 3rd field (_name, _bio, etc) are the lens accessors to the form values
      F.renderForm st.registration Register do
        void $ F.textField     "name"     "User name"    _name         F.nonBlank
        void $ F.textFieldOpt  "bio"      "Bio"          _bio          F.optional
        void $ F.emailField    "email"    "Email"        _email        F.emailValidator
        void $ F.passwordField "password" "Password"     _password     validPassword
        F.passwordField        "confirm"  "Confirmation" _confirmation validConfirmation
      where
        validPassword str
            | Str.length str < 6 = Left "Password must be at least 6 characters"
            | otherwise          = Right str
        validConfirmation p
            | p == st ^. stRegistration <<< _password = Right p
            | otherwise = Left "Password must match confirmation"
```

Full example [here](https://github.com/tippenein/wform/blob/master/example/Main.purs)
