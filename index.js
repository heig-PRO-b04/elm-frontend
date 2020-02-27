import { Elm } from "./src/Main.elm";

// Maybe we will want to pass the API endpoint in the program flags.
const app = Elm.Main.init({
  node: document.getElementById("elm")
});