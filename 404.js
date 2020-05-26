const storageKey = "credentials";
const flags = localStorage.getItem(storageKey) || "{}";

const path = window.location.href;

/* Update the path in the local storage appropriately for redirection. */
let obj = JSON.parse(flags);
obj.redirection = path;
localStorage.setItem(storageKey, JSON.stringify(obj));

/* Navigate to the home page. */
window.location.href = "/"