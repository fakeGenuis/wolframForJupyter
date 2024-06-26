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
            (* allow interrupt *)
            (* FIXME code cell seems be executed line by line in link, *)
            (* which means interrupt continues after line being executed *)
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
    Label[end];
    $session["status"] = "idle";
    rspMsgContent
];

$config = <|"secret" -> "(* pray to god *)", "max_text_length" -> 2 * 10^3|>;

textTruncate[s_String] := Module[{diff = StringLength[s] - $config["max_text_length"]},
    If[diff <= 0, Return[s]];
    StringTemplate["`1`\n...`2`"][
        StringTake[s, $config["max_text_length"]]
        ,
        errWrapper @ StringTemplate["(`1` more characters hide)"][diff]
    ]
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
