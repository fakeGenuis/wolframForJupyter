$debug = <|
    (* larger for more message, not bigger than length of ColoredCodes *)
    "DebugLevel" -> 4
    ,
    (* alert, error, warning, info *)
    "ColoredCodes" -> {"A" -> 36, "E" -> 31, "W" -> 33, "I" -> 32}
    ,
    "ColoredWrapper" -> StringTemplate["\033[;`1`m`2`\033[0m"]
    ,
    "Format" -> (
        StringTemplate["`1` `2`\n"][
            $debug["ColoredWrapper"][#2, StringTemplate["(`1`) [<*DateString[\"ISODateTime\"]*>] `2`"][#1, #3]]
            ,
            #4
        ]&
    )
    ,
    "IsMsg" -> (KeyExistsQ[#, "header"] && KeyExistsQ[#["header"], "msg_type"]&)
|>;

errWrapper[s_String] := $debug["ColoredWrapper"][$debug["ColoredCodes"][[2, -1]], s];

$debugWrite[lvl_Integer, title_String, message_String] := If[
    lvl <= $debug["DebugLevel"]
    ,
    WriteString[
        Streams["stdout"]
        ,
        $debug["Format"][Sequence @@ $debug["ColoredCodes"][[lvl]], title, message]
    ]
];

$debugWrite[lvl_Integer, title_String, expr_] := $debugWrite[lvl, title, ToString[expr]];

$debugWrite[lvl_Integer, msg_Association /; $debug["IsMsg"][msg]] := $debugWrite[lvl, msg["header"]["msg_type"], msg];

$debugWrite[lvl_Integer, msg_] := $debugWrite[lvl, "", msg];

$config = <|
    "secret" -> "(* pray to god *)"
    ,
    "max_text_length" -> 2 * 10^3
    ,
    "complete_min_length" -> 3
|>;

textTruncate[s_String] := Module[{diff = StringLength[s] - $config["max_text_length"]},
    If[diff <= 0, Return[s]];
    StringTemplate["`1`\n...`2`"][
        StringTake[s, $config["max_text_length"]]
        ,
        errWrapper @ StringTemplate["(`1` more characters hide)"][diff]
    ]
];
