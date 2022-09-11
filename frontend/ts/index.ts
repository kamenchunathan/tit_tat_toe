import  "elm-canvas";
import { Context, sendMessage, onReceiveMessage } from "./socketPorts";

let ctx = new Context()

const app = window.Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.sendMsg.subscribe(sendMessage(ctx));
onReceiveMessage(ctx)(app.ports.recvMsg.send);

