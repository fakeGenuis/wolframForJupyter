
(* evaluation session *)

createSession[] := Module[
    {}
    ,
    $session = <|
        "uuid" -> CreateUUID[]
        ,
        "status" -> "starting"
        ,
        "execution_count" -> 1
        ,
        "message" -> None
        ,
        "socketMsg" -> <||>
    |>;
    $session["link"] = LinkLaunch[First[$CommandLine] <> " -wstp"];
    LinkWrite[$session["link"], Unevaluated[EvaluatePacket[$ProcessID]]];
    LinkRead[$session["link"]];
    $session["pid"] = First @ LinkRead[$session["link"]];
    $debugWrite[2, StringTemplate["evaluation kernel (pid: ``) started!"][$session["pid"]]];
    $session["status"] = "idle";
];

(* execute code and send results, return shell channel reply content *)

execHandler[] := Module[
    {
        code = $session["socketMsg"]["content"]["code"]
        ,
        rspMsgContent = <|
            "payload" -> {}
            ,
            "user_expressions" -> <||>
            ,
            "execution_count" -> $session["execution_count"]
        |>
        ,
        shellMsgCache
    }
    ,
    $debugWrite[2, "enter exec handler!"];
    $session["status"] = "busy";
    iopubSend["execute_input", <|"code" -> code|>];
    rspMsgContent["status"] = "ok";
    (* runtime chnage configs of farther process *)
    If[
        StringStartsQ[code, $config["secret"]]
        ,
        pktRsp[ReturnExpressionPacket[ToExpression[code, InputForm]]];
        $session["status"] = "idle";
        Return[rspMsgContent]
    ];
    LinkWrite[$session["link"], EnterTextPacket[code]];
    While[
        True
        ,
        Which[
            SocketReadyQ[$socket["control"]], shellMsgCache = $session["socketMsg"];
            controlHandler[];
            $session["socketMsg"] = shellMsgCache;
            ,
            LinkReadyQ[$session["link"]], pkt = LinkRead[$session["link"]];
            If[
                pkt == $Failed || Head[pkt] === LinkRead
                ,
                $debugWrite[1, "failed to read pkt from link"];
                (* $Failed at frontend, no exception message *)
                rspMsgContent["status"] = "abort";
                Break[];
            ];
            (* FIXME reply to error in LinkWrite *)
            pktRsp[pkt];
            (* If[$session["message"] =!= None, rspMsgContent["status"] = "error"]; *)
        ];
    ];
    $session["status"] = "idle";
    rspMsgContent
];

(* response to pkt read from $session["link"] *)

pktRsp[pkt_] := Switch[
    Head @ pkt
    ,
    InputNamePacket, $session["execution_count"] = First @ StringCases[First @ pkt, RegularExpression["In\\[([0-9]+)\\]"] :> ToExpression["$1"]];
    Break[];
    ,
    OutputNamePacket, $session["execution_count"] = First @ StringCases[First @ pkt, RegularExpression["Out\\[([0-9]+)\\]"] :> ToExpression["$1"]];
    ,
    MessagePacket, $session["message"] = List @@ pkt;
    (*Notify frontend a coming stderr*)
    iopubSend["error", <|"status" -> "error"|>(* , "ids" -> $session["socketMsg"]["ids"] *)];
    ,
    TextPacket, iopubSend[
        "stream", <|"name" -> "stdout", "text" -> First @ pkt|>
        (*FIXME ids here cause `emacs-jupyter' failed rendering stdout*)
        (* , *)
        (* "ids" -> $session["socketMsg"]["ids"] *)
    ];
    $session["message"] = None;
    ,
    MenuPacket, If[
        First @ pkt == 1 && Last @ pkt == "Interrupt> "
        ,
        LinkWrite[$session["link"], "abort"];
        $debugWrite[2, "continue from an interrupt!"];
        ,
        $debugWrite[2, "undefined menu packet!"];
    ];
    ,
    ReturnPacket | ReturnExpressionPacket, iopubSend[
        "execute_result"
        ,
        <|
            "data" -> <|
                (* FIXME add html output *)
                "text/plain" -> ToString @ OutputForm[First @ pkt]
            |>
            ,
            "metadata" -> <||>
        |>
    ];
    ,
    ReturnTextPacket, iopubSend[
        "execute_result"
        ,
        <|
            "data" -> <|"text/plain" -> $config["text_handler"] @ First @ pkt|>
            ,
            "metadata" -> <||>
        |>
    ];
    ,
    _, $debugWrite[2, StringTemplate["unknown packet header: ``"] @ Head[pkt]];
];
