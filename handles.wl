(* if jupyter pending when kernel busy, then parent header always right *)

iopubSend[msgType_String, content_Association] := Module[
    {contNew = content}
    ,
    $debugWrite[3, "send iopub message"];
    Switch[
        msgType
        ,
        "execute_result" | "execute_input" | "error", contNew["execution_count"] = $session["execution_count"];
        ,
        "stream", If[!StringEndsQ[#, "\n"], # = # <> "\n"]& @ contNew["text"];
        If[
            $session["message"] =!= None
            ,
            contNew["name"] = "stderr";
            contNew["text"] = errWrapper @ contNew["text"]
        ];
    ];
    sendMsg[$socket["iopub"], msgNew[msgType, contNew]];
];

(* read request and reply, in shell channel *)

shellHandler[] := Module[
    {type = "unknown", content}
    ,
    $debugWrite[2, "enter shell handler!"];
    $session["socketMsg"] = msgDeserialize @ SocketReadMessage[$socket["shell"], "Multipart" -> True];
    iopubSend["status", <|"execution_state" -> "busy"|>];
    Switch[
        $session["socketMsg"]["header"]["msg_type"]
        ,
        "execute_request", type = "execute_reply";
        content = execHandler[];
        ,
        "kernel_info_request", type = "kernel_info_reply";
        content = $kernelInfo;
        ,
        "complete_request", {type, content} = completeHandler[];
        ,
        "inspect_request", type = "inspect_reply";
        content = inspectHandler[];
        (* FIXME inspect_request and other requests *)
    ];
    sendMsg[$socket["shell"], msgNew[type, content, "ids" -> None]];
    iopubSend["status", <|"execution_state" -> "idle"|>];
];

tryPauseLink[type_String] := TimeConstrained[
    If[type === "close", LinkClose, LinkInterrupt] @ $session["link"];
    $debugWrite[2, type <> " evaluation session succeed!"];
    "ok"
    ,
    5
    ,
    "error"
];

controlHandler[] := Module[
    {type = "unknown", content, shellMsgCache}
    ,
    $debugWrite[2, "enter control handler!"];
    $session["socketMsg"] = msgDeserialize @ SocketReadMessage[$socket["control"], "Multipart" -> True];
    Switch[
        $session["socketMsg"]["header"]["msg_type"]
        ,
        "shutdown_request", type = "shutdown_reply";
        content = $session["socketMsg"]["content"];
        content["status"] = tryPauseLink["close"];
        If[
            content["restart"] && content["status"] != "error"
            ,
            shellMsgCache = $session["socketMsg"];
            createSession[];
            $session["socketMsg"] = shellMsgCache;
            ,
            $session["status"] = "shutdown"
        ];
        ,
        "interrupt_request", If[
            (* only interrupt when kernel is busy *)
            $session["status"] == "busy"
            ,
            type = "interrupt_reply";
            content = <|"status" -> tryPauseLink["interrupt"]|>;
            ,
            $debugWrite[3, "kernel not busy, interrupt ignored!"];
        ];
        ,
        (* only when shell channel lose connect *)
        "kernel_info_request", type = "kernel_info_reply";
        content = $kernelInfo;
        (* FIXME debug request *)
    ];
    sendMsg[$socket["control"], msgNew[type, content, "ids" -> None]];
];
