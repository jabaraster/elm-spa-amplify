module Domain exposing
    ( AppConfig
    , JwtToken
    , KayoinobaAttributeIconUrls
    , PlaceIconUrls
    , SignInUser
    )


type alias PlaceIconUrls =
    { category1 : String
    , category2 : String
    , category3 : String
    }


type alias KayoinobaAttributeIconUrls =
    { taisou : String
    , noutore : String
    , ongaku : String
    , insyokuari : String
    , undou : String
    , free : String
    }


type alias JwtToken =
    String


type alias SignInUser =
    { userId : String
    , jwtToken : JwtToken
    }


type alias AppConfig =
    { codeBase : String
    , graphqlEndpoint : String
    , apiKey : String
    , placeIconUrls : PlaceIconUrls
    , kayoinobaAttributeIconUrls : KayoinobaAttributeIconUrls
    , googleMapApiKey : String
    , user : Maybe SignInUser
    }
