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
    , loginErrorMessage : Maybe String
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
    , signUpErrorMessage : Maybe String
    }


initLoginPageFields : LoginPageFields
initLoginPageFields =
    { loginOrCreateUserForm = LoginForm
    , loginFormUsername = ""
    , loginFormPassword = ""
    , loginErrorMessage = Nothing
    , createUserFormUsername = ""
    , doesCreateUserFormUsernameHaveAtLeast1Character = False
    , createUserFormDisplayName = ""
    , doesCreateUserFormDisplayNameHaveAtLeast1Character = False
    , createUserFormEmailAddress = ""
    , isCreateUserFormEmailAddressValid = False
    , createUserFormPassword = ""
    , doesCreateUserFormPasswordHaveAtLeast10Characters = False
    , doesCreateUserFormPasswordHaveAtMost128Characters = True
    , doesCreateUserFormPasswordHaveALowerCaseCharacter = False
    , doesCreateUserFormPasswordHaveAnUpperCaseCharacter = False
    , doesCreateUserFormPasswordHaveADigit = False
    , createUserFormRepeatPassword = ""
    , doesCreateUserFormRepeatPasswordMatch = True
    , signUpErrorMessage = Nothing
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
        let
            createUserFields =
                { newUsername = loginPageFields.createUserFormUsername
                , newDisplayName = loginPageFields.createUserFormDisplayName
                , newPassword = loginPageFields.createUserFormPassword
                , newEmailAddress = Nothing
                }
        in
        if loginPageFields.createUserFormEmailAddress == "" then
            -- Email is empty, it's optional so that's okay
            Just createUserFields

        else if loginPageFields.isCreateUserFormEmailAddressValid then
            -- Email is nonempty and valid
            Just { createUserFields | newEmailAddress = Just loginPageFields.createUserFormEmailAddress }

        else
            -- Email address is invalid and nonempty
            Nothing
