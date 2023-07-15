import { urls as placeIconUrls } from "./place-icon-url";
import { urls as kayoinobaAttributeIconUrls } from "./kayoinoba-attribute-icon-url";
import { Amplify, Storage } from "aws-amplify";
import awsConfig from "../aws-exports";
Amplify.configure(awsConfig);


import '@webcomponents/webcomponentsjs/custom-elements-es5-adapter';
import '@google-web-components/google-map';

import { Elm } from "../Map.elm";
import { AMPLIFY_STORAGE_LEVEL } from "./const";

function registerSubscriber(ports, functionName, handler) {
    if (!ports) return;
    if (!ports[functionName]) return;
    ports[functionName].subscribe(handler);
}

(function () {
    const elmApp = Elm.Map.init({
        node: document.getElementById("main"),
        flags: {
            graphqlEndpoint: awsConfig.aws_appsync_graphqlEndpoint,
            apiKey: awsConfig.aws_appsync_apiKey,
            placeIconUrls,
            kayoinobaAttributeIconUrls,
            codeBase: location.origin,
            googleMapApiKey: process.env["GOOGLE_MAP_API_KEY"],
            user: null,
        },
    });
    const ports = elmApp.ports;
    registerSubscriber(ports, "requestFileUrls", async (fileNames) => {
        console.log("requestFileUrls")
        const ret = await Promise.all(fileNames.map(async (fileName) => {
            const ret = await Storage.get(fileName, {
                download: false,
                level: AMPLIFY_STORAGE_LEVEL
            });
            return { fileName, url: ret };
        }));
        console.log(ret);
        ports.receiveFileUrls.send(ret);
    });
})();