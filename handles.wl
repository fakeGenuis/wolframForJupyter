
(* if jupyter pending when kernel busy, then parent header always right *)

iopubSend[msgType_String, content_Association] := Module[
    {msg}
    ,
    msg = msgNew[msgType, content];
    $debugWrite[2, "send iopub message"];
    Switch[
        msgType
        ,
        "execute_result" | "execute_input", msg["content"]["execution_count"] = $session["execution_count"];
    ];
    sendMsg[$socket["iopub"], msg];
];

(* read request and reply, in shell channel *)

shellHandler[] := Module[
    {rspMsg}
    ,
    $debugWrite[2, "enter shell handler!"];
    rspMsg = msgNew["unknown", <|"status" -> "ok"|>, "ids" -> $session["socketMsg"]["ids"]];
    Switch[
        $session["socketMsg"]["header"]["msg_type"]
        ,
        "execute_request", rspMsg["header"]["msg_type"] = "execute_reply";
        rspMsg["content"] = execHandler[];
        ,
        "kernel_info_request", rspMsg["header"]["msg_type"] = "kernel_info_reply";
        rspMsg["content"] = $kernelInfo;
        (* FIXME inspect_request and other requests *)
    ];
    If[
        rspMsg["header"]["msg_type"] != "unknown"
        ,
        $debugWrite[3, "send shell message!"];
        sendMsg[$socket["shell"], rspMsg]
    ];
];

controlHandler[] := Module[
    {rspMsg}
    ,
    $debugWrite[2, "enter control handler!"];
    rspMsg = msgNew["unknown", <|"status" -> "ok"|>, "ids" -> $session["socketMsg"]["ids"]];
    Switch[
        $session["socketMsg"]["header"]["msg_type"]
        ,
        "shutdown_request", rspMsg["header"]["msg_type"] = "shutdown_reply";
        rspMsg["content"]["restart"] = $session["socketMsg"]["content"]["restart"];
        TimeConstrained[
            LinkClose[$session["link"]];
            $debugWrite[2, "evaluation session closed!"];
            ,
            5
            ,
            rspMsg["content"]["status"] = "error"
        ];
        If[
            rspMsg["content"]["restart"]
            ,
            createSession[]
            ,
            $debugWrite[1, "quit", "bye-bye!"];
            Quit[]
        ];
        ,
        "interrupt_request", rspMsg["header"]["msg_type"] = "interrupt_reply";
        TimeConstrained[
            LinkInterrupt[$session["link"]];
            $debugWrite[2, "evaluation session interrupt!"];
            ,
            5
            ,
            rspMsg["content"]["status"] = "error"
        ];
        ,
        "kernel_info_request", rspMsg["header"]["msg_type"] = "kernel_info_reply";
        rspMsg["content"] = $kernelInfo;
        (* FIXME debug request *)
    ];
    If[
        rspMsg["header"]["msg_type"] != "unknown"
        ,
        $debugWrite[3, "send control message!"];
        sendMsg[$socket["control"], rspMsg]
    ];
];
