
$debug = <|
    (* larger for more message, not bigger than length of ColoredWrapper *)
    "DebugLevel" -> 4
    ,
    (* alert, error, warning, info *)
    "ColoredWrapper" -> {"A" -> 36, "E" -> 31, "W" -> 33, "I" -> 32}
    ,
    "Format" -> StringTemplate["\033[;`2`m(`1`) [<*DateString[\"ISODateTime\"]*>] `3`\033[0m `4`\n"]
    ,
    "IsMsg" -> (KeyExistsQ[#, "header"] && KeyExistsQ[#["header"], "msg_type"]&)
|>;

$debugWrite[lvl_Integer, title_String, message_String] := If[
    lvl <= $debug["DebugLevel"]
    ,
    WriteString[
        Streams["stdout"]
        ,
        $debug["Format"][Sequence @@ $debug["ColoredWrapper"][[lvl]], title, message]
    ]
];

$debugWrite[lvl_Integer, title_String, expr_] := $debugWrite[lvl, title, ToString[expr]];

$debugWrite[lvl_Integer, msg_Association /; $debug["IsMsg"][msg]] := $debugWrite[lvl, msg["header"]["msg_type"], msg];

$debugWrite[lvl_Integer, msg_] := $debugWrite[lvl, "", msg];
