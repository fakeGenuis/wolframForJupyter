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
    (* skip `In[1]:= ` *)
    $session["pid"] = Part[#, 2, 1]& @ Table[LinkRead[$session["link"]], {2}];
    $debugWrite[1, StringTemplate["evaluation kernel (pid: ``) started!"][$session["pid"]]];
    (* load completion utils, FIXME avoid link into applications *)
    LinkWrite[$session["link"], Unevaluated[EvaluatePacket[Get["stream.wl"];]]];
    $session["status"] = "idle";
];

(* execute code and send results, return shell channel reply content *)

execHandler[] := Module[
    {
        code = $session["socketMsg"]["content"]["code"]
        ,
        rspMsgContent = <|
            "status" -> "ok"
            ,
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
    (* runtime change configs of monitor kernel *)
    If[
        StringStartsQ[code, $config["secret"]]
        ,
        pktRsp[ReturnExpressionPacket[ToExpression[code, InputForm]]];
        Goto[end];
    ];
    (* FIXME only `ReturnTextPacket` valid *)
    LinkWrite[$session["link"], EnterTextPacket[code]];
    (* pkts read loop *)
    While[
        True
        ,
        Which[
            (* stop read pkt in case session restart *)
            $session["status"] != "busy", Break[];
            ,
            (* allow interrupt *)
            (* FIXME code cell seems be executed line by line in link, *)
            (* which means interrupt continues after line being executed *)
            SocketReadyQ[$socket["control"]], shellMsgCache = $session["socketMsg"];
            controlHandler[];
            $session["socketMsg"] = shellMsgCache;
            ,
            (* only handle pkt when kernel is busy, *)
            (* if not, it may continue from a kernel restart *)
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
    Label[end];
    $session["status"] = "idle";
    rspMsgContent
];

(* response to pkt read from $session["link"] *)

pktRsp[pkt_] := Switch[
    Head @ pkt
    ,
    InputNamePacket, $session["execution_count"] = First @ StringCases[First @ pkt, RegularExpression["In\\[([0-9]+)\\]"] :> ToExpression["$1"], 1];
    (* exec finished, or aborted *)
    Break[];
    ,
    OutputNamePacket, $session["execution_count"] = First @ StringCases[First @ pkt, RegularExpression["Out\\[([0-9]+)\\]"] :> ToExpression["$1"], 1];
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
            "data" -> <|"text/plain" -> textTruncate @ First @ pkt|>, "metadata" -> <||>
        |>
    ];
    ,
    _, $debugWrite[2, StringTemplate["unknown packet header: ``"] @ Head[pkt]];
];

$complete = <|
    "end_with_prefix" -> RegularExpression["[a-zA-Z$]+[a-zA-Z0-9$]*"] ~~ EndOfString
    ,
    "start_with_suffix" -> StartOfString ~~ RegularExpression["[a-zA-Z0-9$]*"]
|>;

(* Helper to generate function run in link or kernel *)

genSymbolHandlerF[] := If[
    $session["status"] === "idle"
    ,
    (* Evaluate in link *)
    (
        LinkWrite[
            $session["link"]
            ,
            Unevaluated[EvaluatePacket[ToExpression["wolframForJupyter`Private`" <> #] @@ Rest[{##}]]]
        ];
        First @ LinkRead[$session["link"]]
    )&
    ,
    (* Local evaluation *)
    ToExpression[#] @@ Rest[{##}]&
];


inspectHandler[] := Module[
    {
        content = $session["socketMsg"]["content"]
        ,
        rspMsgContent = <|"status" -> "ok", "metadata" -> <||>|>
        ,
        beforeEnd
        ,
        symbol
        ,
        evalLR = genSymbolHandlerF[]
    }
    ,
    (* FIXME inspect in jupyter browser *)
    content["cursor_pos"] = Min[Length[content["code"]], content["cursor_pos"]];
    beforeEnd = StringTakeDrop[content["code"], content["cursor_pos"]];
    symbol = StringJoin[
        If[# === {}, "", First @ #]& @ StringCases[First @ beforeEnd, $complete["end_with_prefix"], 1]
        ,
        First @ StringCases[Last @ beforeEnd, $complete["start_with_suffix"], 1]
    ];
    If[
        rspMsgContent["found"] = symbol =!= ""
        ,
        rspMsgContent["data"] = <|"text/plain" -> evalLR["docGen", symbol, content["detail_level"]]|>
        ,
        Return[rspMsgContent]
    ];
    rspMsgContent
];

completeHandler[] := Module[
    {
        content = $session["socketMsg"]["content"]
        ,
        beforeEnd
        ,
        rspMsgContent = <|"status" -> "ok", "metadata" -> <||>|>
        ,
        symbolPre
        ,
        evalLR = genSymbolHandlerF[]
    }
    ,
    beforeEnd = StringTakeDrop[content["code"], content["cursor_pos"]];
    symbolPre = If[
        Length @ # == 0 || (StringLength @ First[#] < $config["complete_min_length"])
        ,
        Return[{"unknown", <||>}]
        ,
        First @ #
    ]& @ StringCases[First @ beforeEnd, $complete["end_with_prefix"], 1];
    rspMsgContent["matches"] = evalLR["getNamesFromPrefix", symbolPre];
    rspMsgContent["cursor_start"] = content["cursor_pos"] - StringLength @ symbolPre;
    rspMsgContent["cursor_end"] = content["cursor_pos"] + StringLength @ First @ StringCases[Last @ beforeEnd, $complete["start_with_suffix"], 1];
    rspMsgContent["metadata"]["_jupyter_types_experimental"] = <|
        "text" -> #
        ,
        "type" -> evalLR["symbolType", #]
        ,
        "signature" -> evalLR["symbolSignature", #]
    |>& /@ rspMsgContent["matches"];
    {"complete_reply", rspMsgContent}
];
