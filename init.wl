#!/usr/bin/env wolframscript

BeginPackage["wolframForJupyter`", {"ZeroMQLink`"}];

Begin["`Private`"]

Get /@ {"messages.wl", "debug.wl", "evaluation.wl", "handles.wl"};

$kernelInfo = <|
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
        "version" -> $VersionNumber
        ,
        "mimetype" -> "application/vnd.wolfram.wl"
        ,
        "file_extension" -> ".wl"
        ,
        "pygments_lexer" -> "mathematica"
        ,
        "codemirror_mode" -> "mathematica"
        (* , *)(* "nbconvert_exporter" -> "" *)
    |>
    ,
    "banner" -> $Version
|>;

(* communicate with jupyter *)
(* see https://jupyter-client.readthedocs.io/en/stable/kernels.html# *)

$connection = ToString /@ Association @ Import[$CommandLine[[4]], "JSON"];

$debugWrite[1, "kernel started", $connection];

(* sockets for jupyter connection *)

$socket = AssociationMap[
    SocketOpen[{$connection["ip"], $connection[# <> "_port"]}, {"ZMQ", "Router"}]&
] @ {"control", "shell", "stdin"};

$socket["iopub"] = SocketOpen[{$connection["ip"], $connection["iopub_port"]}, {"ZMQ", "Publish"}];

$socket["hb"] = SocketOpen[{$connection["ip"], $connection["hb_port"]}, {"ZMQ", "Reply"}];

If[
    Or @@ FailureQ /@ $socket
    ,
    $debugWrite[1, "sockets failed to start!"];
    Quit[]
];

(* main socket loop *)

loop[] := Module[
    {readSocket}
    ,
    createSession[];
    $debugWrite[1, "start socket listening loop!"];
    While[
        True
        ,
        If[
            $session["status"] == "shutdown"
            ,
            $debugWrite[1, "quit", "bye-bye!"];
            Quit[]
        ];
        readSocket = First[SocketWaitNext @ Lookup[$socket, {"shell", "control", "hb"}]];
        Switch[
            readSocket
            ,
            (* heart beat: echo back *)
            $socket["hb"], $debugWrite[2, "enter heart beat socket!"];
            BinaryWrite[$socket["hb"], SocketReadMessage @ $socket["hb"]];
            Continue[];
            ,
            $socket["shell"], shellHandler[];
            ,
            $socket["control"], controlHandler[];
            ,
            _, $debugWrite[2, readSocket];
        ];
    ]
];

End[];

EndPackage[];

wolframForJupyter`Private`loop[];
