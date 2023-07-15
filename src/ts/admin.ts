import { urls as placeIconUrls } from "./place-icon-url";
import { urls as kayoinobaAttributeIconUrls } from "./kayoinoba-attribute-icon-url";

import { Amplify, Storage } from "aws-amplify";
import * as Auth from "./amplify-auth";
import { ElmApp } from "./domain";
import awsConfig from "../aws-exports";
Amplify.configure(awsConfig);
Storage.configure(awsConfig);

import { Elm } from "../../.elm-spa/defaults/Main.elm";

const flags = {
    graphqlEndpoint: awsConfig.aws_appsync_graphqlEndpoint,
    apiKey: awsConfig.aws_appsync_apiKey,
    placeIconUrls,
    kayoinobaAttributeIconUrls,
    codeBase: location.origin,
    googleMapApiKey: process.env["GOOGLE_MAP_API_KEY"],
    user: null,
};

window.addEventListener("load", () => {
    Auth.currentAuthenticatedUser()
        .then((user) => {
            console.log(user);
            flags.user = user;
            const elmApp: ElmApp = Elm.Main.init({
                flags: flags
            });
            Auth.iniitializePorts(elmApp);
        })
        .catch((err) => {
            console.log(err);
            const elmApp: ElmApp = Elm.Main.init({
                flags: flags
            });
            Auth.iniitializePorts(elmApp);
        })
        ;
});
