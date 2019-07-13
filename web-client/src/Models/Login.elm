module Models.Login exposing
    ( LoginOrCreateUserForm(..)
    , LoginPageFields
    , initLoginPageFields
    )


type LoginOrCreateUserForm
    = LoginForm
    | CreateUserForm


type alias LoginPageFields =
    { loginOrCreateUserForm : LoginOrCreateUserForm
    , loginFormUsername : String
    , loginFormPassword : String
    , createUserFormUsername : String
    , createUserFormDisplayName : String
    , createUserFormEmailAddress : String
    , createUserFormPassword : String
    , createUserFormRepeatPassword : String
    }


initLoginPageFields : LoginPageFields
initLoginPageFields =
    { loginOrCreateUserForm = LoginForm
    , loginFormUsername = ""
    , loginFormPassword = ""
    , createUserFormUsername = ""
    , createUserFormDisplayName = ""
    , createUserFormEmailAddress = ""
    , createUserFormPassword = ""
    , createUserFormRepeatPassword = ""
    }
