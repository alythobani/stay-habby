module Models.Login exposing
    ( CreateUserFields
    , LoginOrCreateUserForm(..)
    , LoginPageFields
    , extractCreateUserFields
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
    , doesCreateUserFormUsernameHaveAtLeast1Character : Bool
    , createUserFormDisplayName : String
    , doesCreateUserFormDisplayNameHaveAtLeast1Character : Bool
    , createUserFormEmailAddress : String
    , isCreateUserFormEmailAddressValid : Bool
    , createUserFormPassword : String
    , doesCreateUserFormPasswordHaveAtLeast10Characters : Bool
    , doesCreateUserFormPasswordHaveAtMost128Characters : Bool
    , doesCreateUserFormPasswordHaveALowerCaseCharacter : Bool
    , doesCreateUserFormPasswordHaveAnUpperCaseCharacter : Bool
    , doesCreateUserFormPasswordHaveADigit : Bool
    , createUserFormRepeatPassword : String
    , doesCreateUserFormRepeatPasswordMatch : Bool
    }


initLoginPageFields : LoginPageFields
initLoginPageFields =
    { loginOrCreateUserForm = LoginForm
    , loginFormUsername = ""
    , loginFormPassword = ""
    , createUserFormUsername = ""
    , doesCreateUserFormUsernameHaveAtLeast1Character = True
    , createUserFormDisplayName = ""
    , doesCreateUserFormDisplayNameHaveAtLeast1Character = True
    , createUserFormEmailAddress = ""
    , isCreateUserFormEmailAddressValid = True
    , createUserFormPassword = ""
    , doesCreateUserFormPasswordHaveAtLeast10Characters = True
    , doesCreateUserFormPasswordHaveAtMost128Characters = True
    , doesCreateUserFormPasswordHaveALowerCaseCharacter = True
    , doesCreateUserFormPasswordHaveAnUpperCaseCharacter = True
    , doesCreateUserFormPasswordHaveADigit = True
    , createUserFormRepeatPassword = ""
    , doesCreateUserFormRepeatPasswordMatch = True
    }


{-| The data required to create a user.
-}
type alias CreateUserFields =
    { newUsername : String
    , newDisplayName : String
    , newEmailAddress : Maybe String
    , newPassword : String
    }


extractCreateUserFields : LoginPageFields -> Maybe CreateUserFields
extractCreateUserFields loginPageFields =
    if
        not loginPageFields.doesCreateUserFormUsernameHaveAtLeast1Character
            || not loginPageFields.doesCreateUserFormDisplayNameHaveAtLeast1Character
            || not loginPageFields.doesCreateUserFormPasswordHaveAtLeast10Characters
            || not loginPageFields.doesCreateUserFormPasswordHaveAtMost128Characters
            || not loginPageFields.doesCreateUserFormPasswordHaveALowerCaseCharacter
            || not loginPageFields.doesCreateUserFormPasswordHaveAnUpperCaseCharacter
            || not loginPageFields.doesCreateUserFormPasswordHaveADigit
            || not loginPageFields.doesCreateUserFormRepeatPasswordMatch
    then
        Nothing

    else
        case ( loginPageFields.createUserFormEmailAddress, loginPageFields.isCreateUserFormEmailAddressValid ) of
            ( validEmailAddress, True ) ->
                Just
                    { newUsername = loginPageFields.createUserFormUsername
                    , newDisplayName = loginPageFields.createUserFormDisplayName
                    , newEmailAddress = Just validEmailAddress
                    , newPassword = loginPageFields.createUserFormPassword
                    }

            ( "", False ) ->
                -- Email is optional
                Just
                    { newUsername = loginPageFields.createUserFormUsername
                    , newDisplayName = loginPageFields.createUserFormDisplayName
                    , newEmailAddress = Nothing
                    , newPassword = loginPageFields.createUserFormPassword
                    }

            _ ->
                Nothing
