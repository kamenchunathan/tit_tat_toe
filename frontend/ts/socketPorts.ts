type OpenSocketMessage = {
  kind: "open",
  uri: String
};

type ErrorSocketMessage = {
  kind: "error",
  uri: String
};

type CloseSocketMessage = {
  kind: "close",
  uri: String
};

type MsgSocketMessage = {
  kind: "message",
  data: any
}

type SocketMessage = OpenSocketMessage | ErrorSocketMessage | CloseSocketMessage | MsgSocketMessage;

export class Context  {
  sock: WebSocket;
  listener: (any) => void;
}

export const connect = (ctx: Context) =>  (uri: String)  => {
  if (ctx.sock) return;
  
  ctx.sock = new WebSocket(uri);
  
  ctx.sock.addEventListener("open", (event) => {
    console.log(event);
    ctx.listener({
      kind: "open",
      uri: event.currentTarget.url,
    } as OpenSocketMessage);
  }); 
  
  ctx.sock.addEventListener("message", (event) => {
    ctx.listener(event.data);
  }); 
  
  ctx.sock.addEventListener("error", (event) => {
  }); 
  
  ctx.sock.addEventListener("close", (event) => {
  }); 
}

export const onReceiveMessage = (ctx: Context) => (sendToElm) => {
  if (ctx.sock) {
    ctx.sock.addEventListener("message", function(event){
      sendToElm(event.data)
    });  
  } else {
    ctx.listener = sendToElm;
  }
}

export const sendMessage = (ctx: Context) => (message: SocketMessage) => {
  if (message.kind == "open"){
    connect(ctx)(message["uri"])
  } else if (message.kind == "message"){
   console.log(message);
    ctx.sock.send(message);
  }
}
