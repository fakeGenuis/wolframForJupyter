If[
    $Context === "Global`"
    ,
    Begin["wolframForJupyter`Private`"];
    $fromGlobal = True
];

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
    "Format" -> (Composition[
        s |-> StringTemplate["`1` `2`\n"][s, #4]
        ,
        s |-> $ansi["wrapper"][#2, s, ""]
        ,
        StringTemplate["(`1`) [<*DateString[\"ISODateTime\"]*>] `2`"]
    ][#1, #3]&)
    ,
    "IsMsg" -> (KeyExistsQ[#, "header"] && KeyExistsQ[#["header"], "msg_type"]&)
|>;

errWrapper[s_String] := $ansi["wrapper"][$debug["ColoredCodes"][[2, -1]], s, ""];

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
    "doc_PageWidth" -> 4 * 78
    ,
    "complete_min_length" -> 3
|>;

SetOptions[$Output, PageWidth -> $config["doc_PageWidth"]];

textTruncate[s_String] := Module[{diff = StringLength[s] - $config["max_text_length"]},
    If[diff <= 0, Return[s]];
    StringTemplate["`1`\n...`2`"][
        StringTake[s, $config["max_text_length"]]
        ,
        errWrapper @ StringTemplate["(`1` more characters hide)"][diff]
    ]
];

docGen[symbol_String, level_Integer] := Module[
    {usage}
    ,
    usage = StringReplace[
        Information[symbol, "Usage"]
        ,
        RegularExpression["\!\(\*(.+?)\)"] :> toANSI["$1", "36", "plain" -> (level === 0)]
    ];
    StringRiffle[
        StringTemplate["``\n``"][$ansi["wrapper"]["31", #1, ""], #2]& @@@ Join[{{"Usage:", usage}}, If[level === 1, #, {}]]& @ {
            {"Attributes:", Information[symbol, "Attributes"]}
            ,
            {"Options:", ToString @ TableForm @ Options[Symbol @ symbol]}
            ,
            {"Documentation(Web):", Information[symbol, "Documentation"]["Web"]}
        }
        ,
        "\n\n"
    ]
];

(* TODO more type: keyword, module *)

symbolType[s_String] := Which[
    If[
        Context[s] == "Global`"
        ,
        ToExpression[s, InputForm, DownValues]
        ,
        SyntaxInformation @ ToExpression[s]
    ] =!= {}, "function"
    ,
    MemberQ[{"True", "False", "None", "Infinity", "ComplexInfinity"}, s], "constant"
    ,
    True, "variable"
];

symbolSignature[s_String] := "Foo";

(* Get names start with prefix_ *)

getNamesFromPrefix[prefix_] := Module[
    {}
    ,
    (* in running session first, or in system default *)
    If[
        $session["status"] === "idle"
        ,
        $debugWrite[4, "completion from link!"];
        LinkWrite[
            $session["link"]
            ,
            Unevaluated[EvaluatePacket[Flatten[Names[# <> prefix <> "*"]& /@ $ContextPath]]]
        ];
        Part[#, 1]& @ LinkRead[$session["link"]]
        ,
        $debugWrite[4, "completion from kernel!"];
        Flatten[Names[# <> prefix <> "*"]& /@ $ContextPath]
    ]
]

If[$fromGlobal === True, End[]];
