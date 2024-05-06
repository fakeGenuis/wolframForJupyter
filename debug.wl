strRep1By1[s_String, rules_List] := (Composition @@ (r |-> StringReplace[#, r]&) /@ (Reverse @ rules)) @ s;

boxRE = <|
    "SBB" -> RegularExpression[
        (* yeah, nested `StyleBox` only for retaining double quotes, thank you wolfram! *)
        "StyleBox\\[\"\\\\\"\\\\!\\\\\\(\\\\\\*(.+?)\\\\\\)\\\\\"\", ShowStringCharacters->True\\]"
    ]
    ,
    "SB" -> RegularExpression["StyleBox\\[\"(.+?)\", \"(.+?)\"\\]"]
    ,
    "SsB" -> RegularExpression["SubscriptBox\\[\"(.+?)\", \"(.+?)\"\\]"]
    ,
    "sep" -> RegularExpression["\"(}\\])*, (RowBox\\[{)*\""]
|>;

$ansi = <|
    "wrapper" -> StringTemplate["\033[`1`m`2`\033[0;`3`m"]
    ,
    "shrinker" -> (
        s |-> strRep1By1[s, #]& @ {
            RegularExpression["m\033\\["] :> ";"
            ,
            RegularExpression["\033\\[([0-9;]+)m"] :> StringTemplate["\033[``m"][
                StringRiffle[Reverse @ DeleteDuplicates @ Reverse @ StringSplit["$1", ";"], ";"]
            ]
        }
    )
|>;

Options[toANSI] = {"plain" -> False};

toANSI[s_String, color_String, OptionsPattern[]] := Module[
    {wrapper = If[OptionValue["plain"], StringTemplate["`2`"], $ansi["wrapper"]]}
    ,
    Composition[
        $ansi["shrinker"]
        ,
        wrapper[color, #, "0"]&
        ,
        (* hope there never being "Qu0tE" in usage *)
        StringReplace[#, {"," -> ", ", "Qu0tE" -> "\""}]&
        ,
        StringDelete[#, {boxRE["sep"], "RowBox[{\"", "\"}]", "\""}]&
        ,
        strRep1By1[s, #]&
    ] @ {
        boxRE["SBB"] :> StringTemplate["\"Qu0tE\", ``, \"Qu0tE\""][StringReplace[StringDelete["$1", "\\"], "," -> ", "]]
        ,
        boxRE["SB"] :> StringTemplate["\"``\""][wrapper[#, "$1", color]& @ Switch["$2", "TI", "3", "TR", color, _, "5"]]
        ,
        boxRE["SsB"] :> StringTemplate["\"$1``\""] @ wrapper["4", "$2", color]
    }
];

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
