module Domain exposing (JwtToken, SignInUser)


type alias JwtToken =
    String


type alias SignInUser =
    { userId : String
    , jwtToken : JwtToken
    }
