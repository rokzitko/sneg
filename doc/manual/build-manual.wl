(* ::Package:: *)

(* Build the SNEG manual.

     wolframscript -file doc/manual/build-manual.wl [force] [nolatex] [chapter ...]

   Chapters are LaTeX files in doc/manual/chapters. Their code blocks
   \begin{wl} ... \end{wl} are evaluated in a fresh kernel per chapter
   (doc/tools/run-examples.wl) if the cache in doc/manual/cache is missing or
   its code differs from the source ("force" re-evaluates). The formatted
   inputs and outputs are written to doc/manual/generated/<chapter>/<n>.tex,
   which the wl environment includes. Unless "nolatex" is given, the manual
   is typeset with latexmk (LuaLaTeX, biber) to doc/manual/sneg-manual.pdf.
   Chapter names restrict the evaluation to the given chapters. *)

$manDir = DirectoryName[$InputFileName];
$docDir = ParentDirectory[$manDir];
$repoDir = ParentDirectory[$docDir];
args = Rest[$ScriptCommandLine];
force = MemberQ[args, "force"];
nolatex = MemberQ[args, "nolatex"];
only = DeleteCases[args, "force" | "nolatex"];

Get[FileNameJoin[{$docDir, "tools", "SnegDoc.wl"}]];
Block[{Print}, Get[FileNameJoin[{$repoDir, "sneg.m"}]]];

ref = RunProcess[{"python3", FileNameJoin[{$manDir, "reference.py"}]}];
If[ref["ExitCode"] =!= 0, Print[ref["StandardError"]]; Exit[1]];
If[only === {},
  (* The reference quotes inputs from the help. Reuse their checked
     evaluations instead of trusting a listing that may have been edited. *)
  firstInputs = Import[FileNameJoin[{$manDir, "generated", "reference-inputs.json"}], "RawJSON"];
  refJobs = KeyValueMap[Function[{id, code}, Module[{file, cache, r},
    file = FileNameJoin[{$docDir, "src", "ref", id <> ".md"}];
    cache = FileNameJoin[{$docDir, "cache", "ref", id <> ".wl"}];
    r = If[FileExistsQ[cache], Get[cache]["Results"], {}];
    If[Length[r] > 0 && First[r]["Code"] === code, Nothing,
      <|"Name" -> id, "File" -> file, "Cache" -> cache|>]]], firstInputs];
  If[refJobs =!= {} && !SnegDoc`RunExamples[refJobs], Exit[1]]];
chapters = Map[<|"Name" -> FileBaseName[#], "File" -> #,
    "Cache" -> FileNameJoin[{$manDir, "cache", FileBaseName[#] <> ".wl"}]|> &,
  Join[FileNames["*.tex", FileNameJoin[{$manDir, "chapters"}]],
    FileNames["lab-*.tex", FileNameJoin[{$manDir, "generated", "labs"}]]]];
If[only =!= {}, chapters = Select[chapters, MemberQ[only, #["Name"]] &]];
If[Complement[only, Lookup[chapters, "Name"]] =!= {},
  Print["Unknown chapter: ", Complement[only, Lookup[chapters, "Name"]]]; Exit[2]];

blocksOf[c_] := SnegDoc`TeXExampleBlocks[Import[c["File"], "Text", CharacterEncoding -> "UTF-8"]];
cached[c_] := If[FileExistsQ[c["Cache"]], Lookup[#, {"Code", "Reset"}] & /@ Get[c["Cache"]]["Results"], None];
stale = Select[chapters, force || cached[#] =!= blocksOf[#] &];
If[stale =!= {},
  Print["Evaluating code: ", #["Name"] & /@ stale];
  If[!SnegDoc`RunExamples[stale], Exit[1]]];

(* Visible width of a LaTeX formula, roughly in characters. *)
texWidth[s_String] := StringLength[StringReplace[s, {
  RegularExpression["\\\\(left|right|text|mathrm|mathbf)"] -> "",
  RegularExpression["\\\\[a-zA-Z]+"] -> "x", "{" | "}" | "^" | "_" | "&" -> ""}]];

(* Break the parts of an output into lines of at most $lineWidth characters. *)
$lineWidth = 56;
lines[parts_List] := Module[{out = {}, cur = "", w = 0},
  Scan[Function[p, If[w > 0 && w + texWidth[p] > $lineWidth,
      AppendTo[out, cur]; cur = p; w = texWidth[p],
      cur = cur <> " " <> p; w += texWidth[p]]], parts];
  Append[out, cur]];

outputTeX[t_Association] := Module[{parts = t["Parts"], l},
  If[t["Kind"] === "List",
    parts = MapIndexed[If[#2[[1]] < Length[parts], # <> ",", #] &, parts];
    parts = ReplacePart[parts, {1 -> "\\{" <> First[parts], -1 -> Last[parts] <> "\\}"}]];
  l = lines[parts];
  "$\\begin{aligned}[t]&" <> StringRiffle[l, "\\\\\n&\\quad "] <> "\\end{aligned}$"];

(* TeXForm adds an extra divider after the first data row of a headed
   TableForm. Respect the actual row-divider specification in the boxes. *)
formattedOutput[c_] := Module[{tex = outputTeX[c[[4]]]},
  If[StringContainsQ[c[[3]], "//TableForm="] &&
      !FreeQ[c[[2]], HoldPattern["Rows" -> {False, True, {False}, False}]],
    tex = StringReplace[tex, RegularExpression["(\\\\hline\\n[^\\n]*\\n)\\\\hline\\n"] -> "$1"]];
  tex];

cellTeX[c_] := Switch[First[c],
  "Output", "\\begin{wloutput}{" <> StringReplace[c[[3]], RegularExpression["//[A-Za-z]+="] -> "="] <>
    "}\n" <> formattedOutput[c] <> "\n\\end{wloutput}\n",
  "Print", "\\begin{wlprint}\n" <> c[[3]] <> "\n\\end{wlprint}\n",
  "Message", "\\begin{wlmessage}\n" <> c[[2]] <> "\n\\end{wlmessage}\n",
  "Graphics", "\\wlgraphics{" <> c[[3]] <> "}{cache/" <> c[[2]] <> "}\n"];

writeChapter[c_] := Module[{res = Get[c["Cache"]]["Results"], dir = FileNameJoin[{$manDir, "generated", c["Name"]}]},
  If[DirectoryQ[dir], DeleteDirectory[dir, DeleteContents -> True]];
  CreateDirectory[dir, CreateIntermediateDirectories -> True];
  MapIndexed[Function[{r, i},
    Export[FileNameJoin[{dir, ToString[First[i]] <> ".tex"}],
      "\\begin{wlinput}{" <> r["InLabel"] <> "}\n" <>
      StringReplace[r["Code"], ("\\[Epsilon]" | FromCharacterCode[949]) -> "(*@\\char\"03B5{}@*)"] <> "\n\\end{wlinput}\n" <>
        StringJoin[cellTeX /@ r["Cells"]], "Text", CharacterEncoding -> "UTF-8"]], res]];

Scan[If[FileExistsQ[#["Cache"]], writeChapter[#]] &, chapters];

(* Keywords for the code listings: the public SNEG symbols. *)
Export[FileNameJoin[{$manDir, "generated", "keywords.tex"}],
  "\\lstdefinelanguage{SNEG}{morekeywords={" <>
    StringRiffle[Select[Names["Sneg`*"], StringMatchQ[#, RegularExpression["[A-Za-z][A-Za-z0-9]*"]] &], ","] <>
    "}, sensitive=true}\n", "Text"];
Print["Wrote the generated code blocks."];

If[!nolatex,
  r = RunProcess[{"latexmk", "-lualatex", "-interaction=nonstopmode", "-halt-on-error", "sneg-manual.tex"},
    ProcessDirectory -> $manDir];
  If[r["ExitCode"] =!= 0,
    Print[StringTake[r["StandardOutput"], -Min[4000, StringLength[r["StandardOutput"]]]]];
    Print["ERROR: LaTeX failed"]; Exit[1]];
  Print["Wrote ", FileNameJoin[{$manDir, "sneg-manual.pdf"}]]];
