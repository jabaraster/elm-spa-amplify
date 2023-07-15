import "bulma/css/bulma.min.css";

if (location.pathname.startsWith("/admin/") || location.pathname.startsWith("/auth/")) {
    console.log("launch admin");

    const adminScriptTag = document.createElement("script") as HTMLScriptElement;
    adminScriptTag.src = new URL("./admin.ts", import.meta.url).href;
    // adminScriptTag.type = "module";
    appendScriptTag(adminScriptTag);

} else {
    console.log("launch user");
    const userScriptTag = document.createElement("script") as HTMLScriptElement;
    userScriptTag.src = new URL("./user.ts", import.meta.url).href;
    // userScriptTag.type = "module";
    appendScriptTag(userScriptTag);
}


function appendScriptTag(tag: HTMLScriptElement) {
    document.getElementsByTagName("body")[0].appendChild(tag);
}