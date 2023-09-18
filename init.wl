#!/usr/bin/env wolframscript

BeginPackage["wolframForJupyter`", {"ZeroMQLink`"}];

Begin["`Private`"]

(* predefined variables, mostly for code readability *)

$preDef = <|
    "HashMethod" -> "SHA256"
    ,
    (* larger for more message *)
    "DebugLevel" -> 4
    ,
    "BlockSize" -> 64
    ,
    "ColoredWrapper" -> {"A" -> 36, "E" -> 31, "W" -> 33, "I" -> 32}
    ,
    (* mma Association to json string *)
    "Serialize" -> (If[Head[#] === Association, ExportString[#, "JSON", "Compact" -> True], #]&)
    ,
    (* json string to mma Association *)
    "Deserialize" -> (Association @ ImportByteArray[StringToByteArray[#, "UTF-8"], "JSON"]&)
    ,
    "MessageParseRule" -> RegularExpression["(.+)<IDS\\|MSG>(.+?){(.+?)}{(.*?)}{(.*?)}{(.*?)}$"] :> AssociationThread[
        {"idsLen", "signature", "header", "parent_header", "metadata", "content"}
        ,
        {StringLength @ "$1", "$2", "{$3}", "{$4}", "{$5}", "{$6}"}
    ]
|>;

$kernel = <|
    "status" -> "ok"
    ,
    "protocol_version" -> "5.4.0"
    ,
    "implementation" -> "wolframForJupyter"
    ,
    "implementation_version" -> "0.0.1"
    ,
    "language_info" -> <|
        "name" -> "Wolfram Language"
        ,
        "version" -> "13.2"
        ,
        "mimetype" -> "application/vnd.wolfram.m"
        ,
        "file_extension" -> ".m"
        ,
        "pygments_lexer" -> "mathematica"
        ,
        "codemirror_mode" -> "mathematica"
        (* , *)(* "nbconvert_exporter" -> "" *)
    |>
    ,
    "banner" -> "Wolfram Language 13.2.0 Engine for Linux x86 (64-bit)\nCopyright 1988-2022 Wolfram Research, Inc."
|>;

$debugWrite[msg_String, lvl_Integer] := If[
    lvl <= $preDef["DebugLevel"]
    ,
    WriteString[
        Streams["stdout"]
        ,
        StringTemplate["\033[;`2`m(`1`) [`3`]\033[0m `4`\n"][Sequence @@ $preDef["ColoredWrapper"][[lvl]], DateString["ISODateTime"], msg]
    ]
];

$debugWrite[msg_, lvl_Integer] := $debugWrite[ToString[msg, InputForm], lvl];

(* communicate with jupyter *)
(* see https://jupyter-client.readthedocs.io/en/stable/kernels.html# *)

$connection = ToString /@ Association @ Import[$CommandLine[[4]], "JSON"];

$debugWrite[$connection, 1];

(* sockets for jupyter connection *)

$socket = AssociationMap[
    SocketOpen[{$connection["ip"], $connection[# <> "_port"]}, {"ZMQ", "Router"}]&
] @ {"control", "shell", "stdin"};

$socket["iopub"] = SocketOpen[{$connection["ip"], $connection["iopub_port"]}, {"ZMQ", "Publish"}];

$socket["hb"] = SocketOpen[{$connection["ip"], $connection["hb_port"]}, {"ZMQ", "Reply"}];

If[
    Or @@ FailureQ /@ $socket
    ,
    $debugWrite["sockets failed to start!", 1];
    Quit[]
];

(* HMAC signature of a message *)

hmac[key_String, message_String] := Module[
    {baKey, baMessage, baKeyPrime, oPadded, iPadded}
    ,
    (* adapted from wikipedia article on HMAC's definition *)
    {baKey, baMessage} = StringToByteArray /@ {key, message};
    baKeyPrime = If[
        Length[baKey] > $preDef["BlockSize"]
        ,
        Hash[baKey, $preDef["HashMethod"], "ByteArray"]
        ,
        Join[baKey, ByteArray[Table[0, {$preDef["BlockSize"] - Length[baKey]}]]]
    ];
    oPadded = ByteArray[BitXor[#1, 92]& /@ Normal[baKeyPrime]];
    iPadded = ByteArray[BitXor[#1, 54]& /@ Normal[baKeyPrime]];
    Hash[
        Join[oPadded, Hash[Join[iPadded, baMessage], $preDef["HashMethod"], "ByteArray"]]
        ,
        $preDef["HashMethod"]
        ,
        "HexString"
    ]
];

Options[msgNew] = {"ids" -> None, "parent_header" -> <||>, "content" -> <||>};

msgNew[type_String, OptionsPattern[]] := <|
    "ids" -> If[OptionValue["ids"] === None, StringToByteArray @ type, OptionValue["ids"]]
    ,
    "header" -> <|
        "msg_id" -> CreateUUID[]
        ,
        "session" -> $session["uuid"]
        ,
        "username" -> "kernel"
        ,
        "date" -> DateString["ISODateTime"]
        ,
        "msg_type" -> type
        ,
        "version" -> $kernel["protocol_version"]
    |>
    ,
    "parent_header" -> OptionValue["parent_header"]
    ,
    "metadata" -> <||>
    ,
    "content" -> OptionValue["content"]
|>;

(* msg association (in kernel) to msg bytearray (in socket) *)
(* msgInt: msg intermediate, values serialized *)

msgSerialize[msg_Association] := Module[
    {msgInt = msg}
    ,
    $debugWrite["start message serialize!", 3];
    msgInt = AssociationMap[$preDef["Serialize"] @ msg[#]&] @ Keys[msg];
    msgInt["signature"] = hmac[
        $connection["key"]
        ,
        StringJoin @ Lookup[msgInt, {"header", "parent_header", "metadata", "content"}]
    ];
    $debugWrite[msgInt, 4];
    msgInt
];

(* msg bytearray (in socket) to msg association (in kernel) *)

msgDeserialize[socketMsg_ByteArray] := Module[
    {msgInt, signCalc}
    ,
    (* FIXME maybe cases when multi package recived? *)
    $debugWrite["start message deserialize!", 3];
    msgInt = First @ StringCases[$preDef["MessageParseRule"]] @ ByteArrayToString[socketMsg];
    (* msg ids is ByteArray type *)
    msgInt["ids"] = socketMsg[[ ;; msgInt["idsLen"]]];
    signCalc = hmac[
        $connection["key"]
        ,
        StringJoin @ Lookup[msgInt, {"header", "parent_header", "metadata", "content"}]
    ];
    If[
        msgInt["signature"] != signCalc
        ,
        $debugWrite[
            StringTemplate["HMAC signature not match!\nsocket: ``\ncalced: ``"][msg["signature"], signCalc]
            ,
            2
        ]
    ];
    (* deserialize values of msgInt *)
    (msgInt[#] = $preDef["Deserialize"] @ msgInt[#])& /@ {"header", "parent_header", "metadata", "content"};
    $debugWrite[msgInt, 4];
    msgInt
];

sendMsg[socket_, msg_Association] := Module[
    {msgInt = msgSerialize @ msg}
    ,
    ZeroMQLink`ZMQSocketWriteMessage[socket, #, "Multipart" -> True]& /@ {
        msgInt["ids"]
        ,
        StringToByteArray @ "<IDS|MSG>"
        ,
        Sequence @@ (
            StringToByteArray /@ Lookup[msgInt, {"signature", "header", "parent_header", "metadata"}]
        )
    };
    ZeroMQLink`ZMQSocketWriteMessage[socket, StringToByteArray @ msgInt["content"], "Multipart" -> False];
];

iopubSend[msgType_String, content_Association, parentHeader_Association] := Module[
    {msg}
    ,
    $debugWrite["send iopub message: " <> msgType, 4];
    msg = msgNew[
        msgType
        ,
        "ids" -> StringToByteArray @ msgType
        ,
        "content" -> content
        ,
        "parent_header" -> parentHeader
    ];
    Switch[
        msgType
        ,
        "execute_result", msg["content"]["execution_count"] = $session["execution_count"];
    ];
    sendMsg[$socket["iopub"], msg];
];

iopubSend[msgType_String, content_Association] := iopubSend[msgType, content, <||>];

(* evaluation session *)

createSession[] := Module[
    {}
    ,
    $session = <|"uuid" -> CreateUUID[], "status" -> "starting", "execution_count" -> 1|>;
    $session["link"] = LinkLaunch[First[$CommandLine] <> " -wstp"];
    LinkWrite[$session["link"], Unevaluated[EvaluatePacket[$ProcessID]]];
    LinkRead[$session["link"]];
    $session["pid"] = First @ LinkRead[$session["link"]];
    $debugWrite[StringTemplate["evaluation kernel (pid: ``) started!"][$session["pid"]], 2];
    $session["status"] = "idle";
];

(* execute code and send results *)

execHandler[content_Association] := Module[{code = content["code"], rspMsgContent = <||>},
    $debugWrite["enter exec handler!", 2];
    $session["status"] = "busy";
    LinkWrite[$session["link"], EnterTextPacket[code]];
    rspMsgContent["status"] = "ok";
    While[
        True
        ,
        If[
            !LinkReadQ[$session["link"]]
            ,
            Pause[0.05];
            controlHandle[];
            Continue[]
        ];
        pkt = LinkRead[$session["link"]];
        If[
            pkt == $Failed || Head[pkt] === LinkRead
            ,
            $debugWrite["failed to read pkt from link", 1];
            (* $Failed at frontend, no exception message *)
            rspMsgContent["status"] = "abort";
            Break[];
        ];
        (* FIXME reply to error in LinkWrite *)
        pktRsp[pkt];
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
    MessagePacket, iopubSend[
        "stream"
        ,
        <|"name" -> "stderr", "text" -> ToString @ (List @@ pkt)|>
        ,
        $session["socketMsg"]["header"]
    ];
    ,
    TextPacket, iopubSend[
        "stream"
        ,
        <|"name" -> "stdout", "text" -> First @ pkt|>
        ,
        $session["socketMsg"]["header"]
    ];
    ,
    MenuPacket, If[
        First @ pkt == 1 && Last @ pkt == "Interrupt> "
        ,
        LinkWrite[$session["link"], "abort"];
        LinkRead[$session["link"]];
        ,
        $debugWrite["undefined menu packet!", 1];
    ];
    ,
    ReturnPacket | ReturnExpressionPacket, iopubSend[
        "execute_result"
        ,
        <|
            "data" -> <|
                (* FIXME add html output *)
                "text/plain" -> OutputForm[First @ pkt]
            |>
        |>
    ];
    ,
    ReturnTextPacket, iopubSend[
        "execute_result"
        ,
        <|"data" -> <|"text/plain" -> First @ pkt|>|>
        ,
        $session["socketMsg"]["header"]
    ];
    ,
    _, $debugWrite[StringTemplate["unknown packet header: ``"] @ Head[pkt], 1];
];

(* read request and reply, in shell channel *)

shellHandler[msg_Association] := Module[
    {rspMsg}
    ,
    $debugWrite["enter shell handler!", 2];
    rspMsg = msgNew[
        "unknown"
        ,
        "ids" -> msg["ids"]
        ,
        "parent_header" -> msg["header"]
        ,
        "content" -> <|"status" -> "ok"|>
    ];
    Switch[
        msg["header"]["msg_type"]
        ,
        (* FIXME re-broadcast code input *)
        "execute_request", rspMsg["header"]["msg_type"] = "execute_reply";
        rspMsg["content"] = execHandler[msg["content"]];
        ,
        "kernel_info_request", rspMsg["header"]["msg_type"] = "kernel_info_reply";
        rspMsg["content"] = $kernel;
        (* FIXME inspect_request and other requests *)
    ];
    If[
        rspMsg["header"]["msg_type"] != "unknown"
        ,
        $debugWrite["send shell message!", 3];
        sendMsg[$socket["shell"], rspMsg]
    ];
];

controlHandler[msg_Association] := Module[
    {rspMsg}
    ,
    $debugWrite["enter control handler!", 2];
    rspMsg = msgNew[
        "unknown"
        ,
        "ids" -> msg["ids"]
        ,
        "parent_header" -> msg["header"]
        ,
        "content" -> <|"status" -> "ok"|>
    ];
    Switch[
        msg["header"]["msg_type"]
        ,
        "shutdown_request", rspMsg["header"]["msg_type"] = "shutdown_reply";
        rspMsg["content"]["restart"] = msg["content"]["restart"];
        TimeConstrained[
            LinkClose[$session["link"]];
            $debugWrite["evaluation session closed!", 2];
            ,
            5
            ,
            rspMsg["content"]["status"] = "error"
        ];
        If[
            msg["content"]["restart"]
            ,
            createSession[]
            ,
            $debugWrite["jupyter kernel prepear to quit!", 1];
            Quit[]
        ];
        ,
        "interrupt_request", rspMsg["header"]["msg_type"] = "interrupt_reply";
        TimeConstrained[
            LinkInterrupt[$session["link"]];
            $debugWrite["evaluation session interrupt!", 2];
            ,
            5
            ,
            rspMsg["content"]["status"] = "error"
        ];
        ,
        "kernel_info_request", rspMsg["header"]["msg_type"] = "kernel_info_reply";
        (* FIXME debug request *)
    ];
    If[
        rspMsg["header"]["msg_type"] != "unknown"
        ,
        $debugWrite["send control message!", 3];
        sendMsg[$socket["control"], rspMsg]
    ];
];

(* main socket loop *)

loop[] := Module[
    {readSocket}
    ,
    createSession[];
    $debugWrite["start socket listening loop!", 1];
    While[
        True
        ,
        readSocket = First[SocketWaitNext @ Lookup[$socket, {"shell", "control", "hb"}]];
        If[
            readSocket === $socket["hb"]
            ,
            (* heart beat: echo back *)
            $debugWrite["enter heart beat socket!", 2];
            BinaryWrite[$socket["hb"], SocketReadMessage @ $socket["hb"]];
            Continue[];
        ];
        $session["socketMsg"] = msgDeserialize @ SocketReadMessage[readSocket, "Multipart" -> True];
        iopubSend["status", <|"execution_state" -> "busy"|>, $session["socketMsg"]["header"]];
        Switch[
            readSocket
            ,
            $socket["shell"], shellHandler[$session["socketMsg"]];
            ,
            $socket["control"], controlHandler[$session["socketMsg"]];
            ,
            _, $debugWrite[readSocket, 2];
        ];
        iopubSend["status", <|"execution_state" -> "idle"|>, $session["socketMsg"]["header"]];
    ]
];

End[];

EndPackage[];

wolframForJupyter`Private`loop[];
