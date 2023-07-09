import { urls as placeIconUrls } from "./place-icon-url";
import { urls as kayoinobaAttributeIconUrls } from "./kayoinoba-attribute-icon-url";

Elm.Main.init({
    flags: {
        placeIconUrls,
        kayoinobaAttributeIconUrls,
    },
});