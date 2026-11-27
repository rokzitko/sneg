(* Re-evaluate the actual input boxes of one downloadable notebook, in order.
   Run this script in a fresh kernel for each file. *)
Begin["SnegNotebookTest`"];
dir = DirectoryName[$InputFileName];
file = Last[$ScriptCommandLine];
nb = Get[file];
If[!MatchQ[nb, _Notebook], Print["Not a notebook: ", file]; Exit[1]];
(* Load the checkout's package, just as the reader loads the installed paclet.
   Load it before boxes containing package identifiers are parsed. *)
Block[{Print}, Get[FileNameJoin[{dir,"..","..","sneg.m"}]]];
path = DeleteCases[$ContextPath, "SnegNotebookTest`"];
cells = Cases[First[nb], Cell[BoxData[b_], style : ("Input" | "Output"), ___] :> {style,b}, Infinity];
expected = Cases[cells, {"Output",b_} :> b];
actual = {};
failed = False;
SeedRandom[20260101];
Scan[Function[b, Module[{exprs, held, value},
  (* The front end stores a multiline cell as top-level RowBoxes separated
     by newlines; newlines inside one expression remain inside its RowBox. *)
  exprs = If[ListQ[b], DeleteCases[b, "\n"], {b}];
  Scan[Function[box,
    held = Block[{$Context = "Global`", $ContextPath = path}, ToExpression[box, StandardForm, HoldComplete]];
    value = Block[{$Context = "Global`", $ContextPath = path},
      TimeConstrained[Check[ReleaseHold[held], $Failed], 600, $Aborted]];
    If[MemberQ[{$Failed,$Aborted}, value], failed = True];
    If[value =!= Null, AppendTo[actual, ToBoxes[value]]]], exprs]]],
  Cases[cells, {"Input",b_} :> b]];
If[failed || actual =!= expected,
  Print[FileNameTake[file], ": expected ",Length[expected]," outputs; got ",Length[actual]];
  If[Length[actual] === Length[expected],
    Print["Changed outputs: ", Select[Range[Length[actual]], actual[[#]] =!= expected[[#]] &]]];
  Exit[1]];
Print[FileNameTake[file], ": ", Length[actual], " outputs match."];
End[];
