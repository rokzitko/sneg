(* ::Package:: *)

(* Evaluate the examples of one documentation source file in a fresh kernel
   and write the results to a cache file.

     wolframscript -file doc/tools/run-examples.wl source.md cache.wl

   The cache contains, for every code block, the code, the input label, and
   the output, print, and message cells. Each block is split into top-level
   expressions as in a notebook input cell: expressions ending with ';' give
   no output, and every expression advances the line number. Line numbers are
   reset at the example section headings and delimiters, like $Line = 0 in
   documentation notebooks. The exit status is 1 if an example fails to parse,
   is aborted, or times out. *)

(* The runner's own symbols are kept out of Global`, where the examples are
   evaluated. *)
Begin["SnegDocRun`"];

$toolsDir = DirectoryName[$InputFileName];
$repoDir = ParentDirectory[ParentDirectory[$toolsDir]];

(* A kernel launched directly with -script does not set $ScriptCommandLine.
   Accept the same two arguments after the script in either invocation. *)
scriptArgs = If[$ScriptCommandLine === {},
  Replace[$CommandLine, {___, "-script", _, rest___} :> {rest}], Rest[$ScriptCommandLine]];
{src, cacheFile} = Replace[scriptArgs, {
  {a_, b_} :> {a, b},
  _ :> (Print["usage: run-examples.wl source.md cache.wl"]; Exit[2])}];

(* Manual chapters (.tex) also get LaTeX forms of the outputs and PDF files
   of graphics, in the directory <cache>-fig next to the cache file. *)
$tex = StringEndsQ[src, ".tex"];
Block[{$ContextPath = $ContextPath},
  Get[FileNameJoin[{$toolsDir, "SnegDoc.wl"}]];
  If[$tex,
    blocks = SnegDoc`TeXExampleBlocks[Import[src, "Text", CharacterEncoding -> "UTF-8"]],
    doc = Catch[Check[SnegDoc`ParseDocFile[src], SnegDoc`DocError["messages while parsing"]], SnegDoc`DocError];
    If[MatchQ[doc, _SnegDoc`DocError], Print["ERROR: ", src, ": ", First[doc]]; Exit[1]];
    blocks = SnegDoc`ExampleBlocks[doc]]];
$ContextPath = DeleteCases[$ContextPath, "SnegDoc`"];

Block[{$Context = "Global`"}, Get[FileNameJoin[{$repoDir, "sneg.m"}]]];
$exampleContextPath = Join[{"Global`"}, DeleteCases[$ContextPath, "SnegDocRun`" | "Global`"]];
$timeLimit = 600;
SeedRandom[20260101];

(* Messages are written to a temporary file in OutputForm, as they would
   appear in a notebook, and read after each expression. *)
$msgFile = CreateFile[];
readMessages[] := Module[{text},
  Close[$msgStream];
  text = Import[$msgFile, "Text", CharacterEncoding -> "UTF-8"];
  $msgStream = OpenWrite[$msgFile, FormatType -> OutputForm, PageWidth -> Infinity, CharacterEncoding -> "UTF-8"];
  $Messages = {$msgStream};
  Select[StringTrim /@ StringSplit[If[StringQ[text], text, ""], RegularExpression["\n\\s*\n"]], # =!= "" &]];
$msgStream = OpenWrite[$msgFile, FormatType -> OutputForm, PageWidth -> Infinity, CharacterEncoding -> "UTF-8"];
$failed = False;

$figDir = FileNameJoin[{DirectoryName[cacheFile], FileBaseName[cacheFile] <> "-fig"}];
graphicsQ[r_] := MatchQ[r, _Graphics | _Graphics3D | _Legended | _GraphicsGrid | _GraphicsRow | _GraphicsColumn | _Image];

(* LaTeX form of an output, split into parts at which lines can be broken:
   the terms of a sum or the elements of a list. *)
texParts[r_] := Module[{x = Replace[r, (TraditionalForm | StandardForm | OutputForm)[y_] :> y]},
  Which[
    Head[x] === Plus,
      <|"Kind" -> "Sum", "Parts" -> MapIndexed[Function[{t, i}, With[{u = snegtex[t]},
        If[i === {1} || StringStartsQ[u, "-"], u, "+" <> u]]], List @@ x]|>,
    ListQ[x] && !MatrixQ[x] && Length[x] > 1, <|"Kind" -> "List", "Parts" -> Map[snegtex, x]|>,
    True, <|"Kind" -> "Single", "Parts" -> {snegtex[x]}|>]];

formLabel[r_] := Replace[Head[r], {
  h : (TraditionalForm | StandardForm | InputForm | FullForm | MatrixForm | TableForm | TreeForm | OutputForm | Short | Shallow) :>
    "//" <> SymbolName[h], _ -> ""}];

line = 0;
results = Reap[Do[Module[{code = blk[[1]], held, n0, cells = {}, msgs},
  If[blk[[2]], line = 0];
  n0 = line + 1;
  held = Block[{$Context = "Global`", $ContextPath = $exampleContextPath},
    Quiet[Check[ImportString[code, {"WL", "HeldExpressions"}], $Failed]]];
  If[!ListQ[held] || held === {},
    Print["ERROR: cannot parse example in ", src, ":\n", code]; $failed = True; held = {}];
  Do[Module[{n = n0 + k - 1, r, h = held[[k]], quiet},
    $Line = n;
    quiet = MatchQ[h, HoldComplete[CompoundExpression[___, Null]]];
    $Messages = {$msgStream};
    r = Block[{$Context = "Global`", $ContextPath = $exampleContextPath,
        Print = Function[Null, AppendTo[cells, If[$tex,
          {"Print", ToBoxes[Row[{##}]], ToString[Row[{##}]]}, {"Print", ToBoxes[Row[{##}]]}]]]},
      Replace[h, HoldComplete[e_] :> TimeConstrained[CheckAbort[e, $Aborted], $timeLimit, $TimedOut]]];
    msgs = readMessages[];
    cells = Join[cells, Map[{"Message", #} &, msgs]];
    If[r === $Aborted || r === $TimedOut,
      Print["ERROR: ", r, " in example in ", src, ":\n", code]; $failed = True];
    Unprotect[Out]; Out[n] = r; Protect[Out];
    If[!quiet && r =!= Null, With[{label = "Out[" <> ToString[n] <> "]" <> formLabel[r] <> "="},
      AppendTo[cells, Which[
        !$tex, {"Output", ToBoxes[r], label},
        graphicsQ[r], With[{f = ToString[n] <> ".pdf"},
          Quiet[CreateDirectory[$figDir]];
          UsingFrontEnd[Export[FileNameJoin[{$figDir, f}], r]];
          {"Graphics", FileBaseName[cacheFile] <> "-fig/" <> f, label}],
        True, {"Output", ToBoxes[r], label, texParts[r]}]]]];
    line = n],
    {k, Length[held]}];
  If[held === {}, line = n0];
  If[MemberQ[cells, {"Message", _}],
    Print["NOTE: messages in example in ", FileNameTake[src], ": ", Cases[cells, {"Message", m_} :> m]]];
  Sow[<|"Code" -> code, "Reset" -> blk[[2]], "InLabel" -> "In[" <> ToString[n0] <> "]:=", "Cells" -> cells|>]],
  {blk, blocks}]][[2]];
results = If[results === {}, {}, First[results]];

Close[$msgStream];
Quiet[DeleteFile[$msgFile]];
$Messages = {OutputStream["stdout", 1]};

If[$failed, Exit[1]];

Block[{$CharacterEncoding = "ASCII", $ContextPath = {"System`", "Global`"}, $Context = "Global`"},
  Put[<|"Source" -> FileNameTake[src], "SnegVersion" -> $SnegVersion, "Results" -> results|>, cacheFile]];
Print["Wrote ", cacheFile, " (", Length[results], " blocks)"];

End[];
