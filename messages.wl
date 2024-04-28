(* predefined variables, mostly for code readability *)

$message = <|
    "HashMethod" -> "SHA256"
    ,
    "BlockSize" -> 64
    ,
    (* mma Association to json string *)
    "Serialize" -> (If[Head[#] === Association, ExportString[#, "JSON", "Compact" -> True], #]&)
    ,
    (* json string to mma Association *)
    "Deserialize" -> (Association @ ImportByteArray[StringToByteArray[#, "UTF-8"], "JSON"]&)
    ,
    "ParseRule" -> RegularExpression["(.+)<IDS\\|MSG>(.+?){(.+?)}{(.*?)}{(.*?)}{(.*?)}$"] :> AssociationThread[
        {"idsLen", "signature", "header", "parent_header", "metadata", "content"}
        ,
        {StringLength @ "$1", "$2", "{$3}", "{$4}", "{$5}", "{$6}"}
    ]
    ,
    "HMACNotMatch" -> StringTemplate["HMAC signature not match!\nsocket: ``\ncalced: ``"]
|>;

(* HMAC signature of a message *)

hmac[key_String, message_String] := Module[
    {baKey, baMessage, baKeyPrime, oPadded, iPadded}
    ,
    (* adapted from wikipedia article on HMAC's definition *)
    {baKey, baMessage} = StringToByteArray /@ {key, message};
    baKeyPrime = If[
        Length[baKey] > $message["BlockSize"]
        ,
        Hash[baKey, $message["HashMethod"], "ByteArray"]
        ,
        Join[baKey, ByteArray[Table[0, {$message["BlockSize"] - Length[baKey]}]]]
    ];
    oPadded = ByteArray[BitXor[#1, 92]& /@ Normal[baKeyPrime]];
    iPadded = ByteArray[BitXor[#1, 54]& /@ Normal[baKeyPrime]];
    Hash[
        Join[oPadded, Hash[Join[iPadded, baMessage], $message["HashMethod"], "ByteArray"]]
        ,
        $message["HashMethod"]
        ,
        "HexString"
    ]
];

Options[msgNew] = {"ids" -> "", "metadata" -> <||>};

msgNew["unknown", content_Association, OptionsPattern[]] := None;

msgNew[type_String, content_Association, OptionsPattern[]] := <|
    (* "": iopub, None: shell | control *)
    "ids" -> Switch[
        OptionValue["ids"]
        ,
        "", StringToByteArray @ type
        ,
        None, $session["socketMsg"]["ids"]
        ,
        _, OptionValue["ids"]
    ]
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
        "version" -> $kernelInfo["protocol_version"]
    |>
    ,
    (* not empty only when request received *)
    "parent_header" -> $session["socketMsg"]["header"]
    ,
    "metadata" -> OptionValue["metadata"]
    ,
    "content" -> content
|>;

(* msg association (in kernel) to msg bytearray (in socket) *)

msgSerialize[msg_Association] := Module[
 (* msgInt: intermediate msg with its values serialized *)
    {msgInt = Map[$message["Serialize"], msg]}
    ,
    $debugWrite[3, "start message serialize!"];
    msgInt["signature"] = hmac[
        $connection["key"]
        ,
        StringJoin @ Lookup[msgInt, {"header", "parent_header", "metadata", "content"}]
    ];
    $debugWrite[4, msg["header"]["msg_type"], msgInt];
    msgInt
];

(* msg bytearray (in socket) to msg association (in kernel) *)

msgDeserialize[socketMsg_ByteArray] := Module[
    {msgInt, signCalc}
    ,
    (* FIXME maybe cases when multi package recived? *)
    $debugWrite[3, "start message deserialize!"];
    msgInt = First @ StringCases[Quiet @ ByteArrayToString[socketMsg], $message["ParseRule"], 1];
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
        $debugWrite[2, $message["HMACNOTMATCH"][msg["signature"], signCalc]]
    ];
    (* deserialize values of msgInt *)
    msgInt = MapAt[
        $message["Deserialize"]
        ,
        List /@ {"header", "parent_header", "metadata", "content"}
    ] @ msgInt;
    $debugWrite[4, msgInt];
    msgInt
];

sendMsg[socket_, msg_Association] := Module[
    {msgSerial = msgSerialize @ msg}
    ,
    ZeroMQLink`ZMQSocketWriteMessage[socket, #, "Multipart" -> True]& /@ {
        msgSerial["ids"]
        ,
        StringToByteArray @ "<IDS|MSG>"
        ,
        Sequence @@ (
            StringToByteArray /@ Lookup[msgSerial, {"signature", "header", "parent_header", "metadata"}]
        )
    };
    (* absurd here, mark "Multipart" end? *)
    ZeroMQLink`ZMQSocketWriteMessage[socket, StringToByteArray @ msgSerial["content"], "Multipart" -> False];
];
