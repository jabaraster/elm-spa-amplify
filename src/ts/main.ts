import { urls as placeIconUrls } from "./place-icon-url";
import { urls as kayoinobaAttributeIconUrls } from "./kayoinoba-attribute-icon-url";

import { Amplify, Auth, Storage } from "aws-amplify";
import awsConfig from "../aws-exports";
Amplify.configure(awsConfig);
Storage.configure(awsConfig);

import { Elm } from "../../.elm-spa/defaults/Main.elm";

const flags = {
    placeIconUrls,
    kayoinobaAttributeIconUrls,
    user: null,
};

const initializeElmApp = (elmApp: ElmApp) => {
    const ports = elmApp.ports;
    ports.tryLogin.subscribe((user) => {
        if (user.id === "login") {
            ports.succeededLogin.send(user);
        } else {
            ports.failedLogin.send({});
        }
    });
};

interface ElmApp {
    ports: any;
}

Auth.currentAuthenticatedUser()
    .then((user) => {
        flags.user = user;
        const elmApp: ElmApp = Elm.Main.init({
            flags: flags
        });
        initializeElmApp(elmApp);
    })
    .catch((err) => {
        const elmApp: ElmApp = Elm.Main.init({
            flags: flags
        });
        initializeElmApp(elmApp);
    })
    ;
