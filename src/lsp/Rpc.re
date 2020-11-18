open Infix;

type jsonrpc =
  | Message(Json.t, string, Json.t)
  | Notification(string, Json.t)
  | Response(Json.t, Json.t);

let messageFromJson = json => {
  let id = Json.get("id", json);
  let method = Json.get("method", json) |?> Json.string;
  let result = Json.get("result", json);
  let params = Json.get("params", json);
  switch (id) {
  | None =>
    Notification(
      method |! "method required",
      params |! "params required for notification",
    )
  | Some(id) =>
    switch (method, result) {
    | (_, Some(result)) => Response(id, result)
    | (Some(method), _) => Message(id, method, params |! "params required")
    | (None, None) => failwith("Either method or result required")
    }
  };
};

let readMessage = (input) => {
  let clength = input_line(input);
  let cl = "Content-Length: ";
  let cll = String.length(cl);
  if (String.sub(clength, 0, cll) == cl) {
    /* if on windows, dont need the extra -1 */
    let offset = Sys.os_type == "Win32" ? 0 : (-1); /* -1 for trailing \r */
    let num =
      String.sub(clength, cll, String.length(clength) - cll + offset);
    /* log("Num bytes to read: " ++ String.escaped(num)); */
    let num = (num |> int_of_string) + (Sys.os_type == "Win32" ? 1 : 2);
    let buffer = Buffer.create(num);
    Buffer.add_channel(buffer, input, num);
    let raw = Buffer.contents(buffer);
    Log.log("Read message " ++ raw);
    let json =
      try(Json.parse(raw)) {
      | Failure(message) =>
        failwith("Unable to parse message " ++ raw ++ " as json: " ++ message)
      | err =>
        failwith(
          "Other failure " ++ raw ++ " message " ++ Printexc.to_string(err),
        )
      };
    messageFromJson(json);
  } else {
    failwith("Invalid header");
  };
};

let send = (output, content) => {
  let length = String.length(content);
  let sep = Sys.os_type == "Unix" ? "\r\n\r\n" : "\n\n";
  output_string(
    output,
    "Content-Length: " ++ string_of_int(length) ++ sep ++ content,
  );
  flush(output);
};

let sendMessage = (output, id, result) => {
  open JsonShort;
  let content =
    Json.stringify(
      o([("id", id), ("jsonrpc", s("2.0")), ("result", result)]),
    );
  Log.log("Sending response " ++ content);
  send(output, content);
};

let sendError = (output, id, error) => {
  open JsonShort;
  let content =
    Json.stringify(
      o([("id", id), ("jsonrpc", s("2.0")), ("error", error)]),
    );
  Log.log("Sending response " ++ content);
  send(output, content);
};

let sendNotification = (output, method, params) => {
  open JsonShort;
  let content =
    Json.stringify(
      o([
        ("jsonrpc", s("2.0")),
        ("method", s(method)),
        ("params", params),
      ]),
    );
  Log.log("Sending notification " ++ content);
  send(output, content);
};

let serverReqNum = ref(0);

let sendRequest = (log, output, method, params) => {
  open JsonShort;
  serverReqNum := serverReqNum^ + 1;
  let content =
    Json.stringify(
      o([
        ("id", s("server-" ++ string_of_int(serverReqNum^))),
        ("jsonrpc", s("2.0")),
        ("method", s(method)),
        ("params", params),
      ]),
    );
  log("Sending server request " ++ content);
  send(output, content);
};
