import { Elm } from "./src/Main.elm";

// Maybe we will want to pass the API endpoint in the program flags.
const storageKey = "credentials";

const flags = localStorage.getItem(storageKey);
const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: flags
});



app.ports.portStoreCredentials.subscribe(function (val) {
  if (val == null) {
    localStorage.removeItem(storageKey);
  } else {
    localStorage.setItem(storageKey, JSON.stringify(val));
  }

  setTimeout(function () {
    app.ports.portSubscribeCredentials.send(val);
  }, 0);
});

window.addEventListener("storage", function (event) {
  if (event.storageArea === localStorage && event.key === storageKey) {
    app.ports.portSubscribeCredentials.send(event.newValue);
  }
});