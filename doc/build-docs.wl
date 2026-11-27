(* ::Package:: *)

(* Build the SNEG documentation from the sources in doc/src.

     wolframscript -file doc/build-docs.wl [force] [nobuild] [name ...]

   1. The examples of every page are evaluated (doc/tools/run-examples.wl, one
      fresh kernel per page) if the cache in doc/cache is missing or its code
      differs from the source; "force" re-evaluates all selected pages.
   2. Documentation source notebooks (DocumentationTools format) are written
      to build/docsrc/SNEG/Documentation/English.
   3. Unless "nobuild" is given, PacletDocumentationBuild turns them into help
      pages in build/docbuild, which are copied to Documentation/ in the
      repository, so that the help pages work when the checkout itself is used
      as a paclet (PacletDirectoryLoad or a link in the Applications
      directory). build-paclet.m builds the release paclet from the source
      notebooks.

   Names restrict steps 1 and 2 to the given pages; all pages are still known
   for linking. *)

$docDir = DirectoryName[$InputFileName];
$repoDir = ParentDirectory[$docDir];
$srcPaclet = FileNameJoin[{$repoDir, "build", "docsrc", "SNEG"}];
$outDir = FileNameJoin[{$srcPaclet, "Documentation", "English"}];
$docBuildDir = FileNameJoin[{$repoDir, "build", "docbuild"}];
args = Rest[$ScriptCommandLine];
force = MemberQ[args, "force"];
nobuild = MemberQ[args, "nobuild"];
only = DeleteCases[args, "force" | "nobuild"];

Get[FileNameJoin[{$docDir, "tools", "SnegDoc.wl"}]];
Block[{Print}, Get[FileNameJoin[{$repoDir, "sneg.m"}]]];

SnegDoc`$SnegPublicNames = Names["Sneg`*"];

kinds = {
  <|"Kind" -> "ref", "Src" -> "ref", "Out" -> {"ReferencePages", "Symbols"}|>,
  <|"Kind" -> "guide", "Src" -> "guide", "Out" -> {"Guides"}|>,
  <|"Kind" -> "tutorial", "Src" -> "tutorial", "Out" -> {"Tutorials"}|>};

pages = Flatten[Map[Function[k, Map[
  Append[k, {"Name" -> FileBaseName[#], "File" -> #,
    "Cache" -> FileNameJoin[{$docDir, "cache", k["Src"], FileBaseName[#] <> ".wl"}]}] &,
  FileNames["*.md", FileNameJoin[{$docDir, "src", k["Src"]}]]]], kinds]];

(* These symbol pairs differ only by case on common paclet filesystems. *)
symbolOf[id_] := Lookup[<|"BasisVariable" -> "BASIS", "VacuumKet" -> "VACUUM"|>, id, id];
SnegDoc`$DocPages = Association[Map[symbolOf[#["Name"]] -> "paclet:SNEG/ref/" <> #["Name"] &,
  Select[pages, #["Kind"] === "ref" &]]];

If[only =!= {},
  If[!SubsetQ[#["Name"] & /@ pages, only], Print["ERROR: unknown pages ", Complement[only, #["Name"] & /@ pages]]; Exit[1]];
  pages = Select[pages, MemberQ[only, #["Name"]] &]];

parse[p_] := Module[{d = Catch[Check[SnegDoc`ParseDocFile[p["File"]], SnegDoc`DocError["messages while parsing"]], SnegDoc`DocError]},
  If[MatchQ[d, _SnegDoc`DocError], Print["ERROR: ", p["File"], ": ", First[d]]; Exit[1]];
  d];

docs = Association[Map[#["Name"] -> parse[#] &, pages]];

cacheCodes[file_] := If[FileExistsQ[file], Lookup[#, {"Code", "Reset"}] & /@ Get[file]["Results"], None];

stale = Select[pages, Function[p, p["Kind"] =!= "guide" &&
  (force || cacheCodes[p["Cache"]] =!= SnegDoc`ExampleBlocks[docs[p["Name"]]])]];

If[stale =!= {},
  Print["Evaluating examples: ", #["Name"] & /@ stale];
  If[!SnegDoc`RunExamples[stale], Exit[1]]];

writeNotebook[nb_, file_] := (
  Quiet[CreateDirectory[DirectoryName[file], CreateIntermediateDirectories -> True]];
  Block[{$CharacterEncoding = "ASCII", $ContextPath = {"System`"}, $Context = "Global`"}, Put[nb, file]]);

If[only === {} && DirectoryQ[$outDir], DeleteDirectory[$outDir, DeleteContents -> True]];
Quiet[CreateDirectory[$srcPaclet, CreateIntermediateDirectories -> True]];
CopyFile[FileNameJoin[{$repoDir, "PacletInfo.wl"}], FileNameJoin[{$srcPaclet, "PacletInfo.wl"}], OverwriteTarget -> True];

failed = UsingFrontEnd[Select[pages, Function[p, Module[{doc = docs[p["Name"]], cache, nb},
    cache = If[p["Kind"] === "guide", {}, Get[p["Cache"]]["Results"]];
    nb = Catch[Check[Switch[p["Kind"],
      "ref", SnegDoc`ReferenceNotebook[doc, cache],
      "guide", SnegDoc`GuideNotebook[doc],
      "tutorial", SnegDoc`TechNoteNotebook[doc, cache]], SnegDoc`DocError["messages while building the notebook"]], SnegDoc`DocError];
    If[MatchQ[nb, _SnegDoc`DocError],
      Print["ERROR: ", p["File"], ": ", First[nb]]; True,
      writeNotebook[nb, FileNameJoin[Join[{$outDir}, p["Out"], {p["Name"] <> ".nb"}]]]; False]]]]];

If[failed =!= {}, Exit[1]];
Print["Wrote ", Length[pages], " notebooks to ", $outDir];

If[!nobuild,
  Needs["PacletTools`"];
  (* The incremental build keeps pages whose sources were removed, so a full
     build starts from scratch. *)
  If[only === {} && DirectoryQ[$docBuildDir], DeleteDirectory[$docBuildDir, DeleteContents -> True]];
  result = PacletTools`PacletDocumentationBuild[$srcPaclet, $docBuildDir];
  If[!MatchQ[result, _Success], Print[result]; Print["ERROR: documentation build failed"]; Exit[1]];
  With[{built = FileNameJoin[{$docBuildDir, "SNEG", "Documentation"}], target = FileNameJoin[{$repoDir, "Documentation"}]},
    If[DirectoryQ[target], DeleteDirectory[target, DeleteContents -> True]];
    CopyDirectory[built, target]];
  Print["Built help pages in ", FileNameJoin[{$repoDir, "Documentation"}]]];
