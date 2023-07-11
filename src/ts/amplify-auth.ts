import { Auth } from "aws-amplify";
import { CognitoUser } from "amazon-cognito-identity-js";
import { ope } from "./port-helper";
import { ElmApp } from "./domain";
import awsConfig from "../aws-exports";
Auth.configure(awsConfig);


interface SimpleUserData {
    userId: string;
    email: string;
    jwtToken: string;
}

function simplifyCognitoUser(user: CognitoUser): SimpleUserData {
    return {
        userId: user.getUsername(),
        jwtToken: user.getSignInUserSession()!.getAccessToken().getJwtToken()!,
    };
}

export const currentAuthenticatedUser = Auth.currentAuthenticatedUser;

export const iniitializePorts = (elmApp: ElmApp) => {
    const ports = elmApp.ports;

    let signInUser: CognitoUser | null = null;
    ports.signIn.subscribe(async ({ userId, password }) => {
        try {
            signInUser = await Auth.signIn(userId, password);
            if (!signInUser) {
                ports.failSignin.send({ code : "UNKNOWN", message: "原因不明の失敗." });
                return;
            }
            const user = signInUser!;
            if (user.challengeName === "NEW_PASSWORD_REQUIRED") {
                ports.requireChangePassword.send(JSON.stringify(user));
            } else {
                ports.succeedSignIn.send(simplifyCognitoUser(user));
            }
        } catch (err) {
            console.log(err);
            ports.failSignIn.send({ code: err.code || err.message, message: err.message });
        }
    });
    ports.changePassword.subscribe(async ({ newPassword }) => {
        try {
            if (!signInUser) {
                ports.redirectToSignIn.send({});
                return;
            }
            const ret = await Auth.completeNewPassword(signInUser, newPassword);
            ports.succeedChangePassword.send(simplifyCognitoUser(ret));

        } catch (err) {
            console.log(err);
            ports.failChangePassword.send({ code: err.code || err.message, message: err.message });
        }
    });
    ports.forgotPassword.subscribe(async ({ userId }) => {
        console.log(userId);
        try {
            const ret = await Auth.forgotPassword(userId);
            console.log(ret);
            ports.succeedForgotPassword.send({});
        } catch (err) {
            console.log(err);
            ports.failForgotPassword.send({ code: err.code || err.message, message: err.message });
        }
    });
    ports.resetPassword.subscribe(async ({ userId, code, password }) => {
        try {
            const ret = await Auth.forgotPasswordSubmit(userId, code, password);
            console.log(ret);
            ports.succeedResetPassword.send({});
        } catch (err) {
            console.log(err);
            ports.failResetPassword.send({ code: err.code || err.message, message: err.message });
        }
    });
    if (ports.signOut) {
        ports.signOut.subscribe(async () => {
            ope({
                operation: async () => {
                    return await Auth.signOut();
                },
                successResponseConverter: () => null,
                successPortFunc: ports.succeedSignOut,
            });
        });
    }
};