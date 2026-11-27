(* ::Package:: *)

(* Verify the documentation examples.

     wolframscript -file doc/test-docs.m [name ...]

   The examples of every reference page and tech note in doc/src and every
   manual chapter in doc/manual/chapters are
   evaluated in fresh kernels (doc/tools/run-examples.wl) and compared with the
   cached outputs in doc/cache, which are shown in the help pages. A page fails
   if an example fails to parse, is aborted, or times out, if its cache is
   missing or does not belong to the current source, or if any output, printed
   output, or message differs from the cache. After intended changes of the
   outputs, update the cache with
     wolframscript -file doc/build-docs.wl force name ...
   and review the differences with git diff doc/cache. *)

$docDir = DirectoryName[$InputFileName];
Get[FileNameJoin[{$docDir, "tools", "SnegDoc.wl"}]];

(* Display equations nested in list items must not shift the delimiters of
   subsequent inline math (spintensor, stevens, and other reference pages). *)
If[SnegDoc`ParseDocString["# Check\n- Defined by\n  $$T = S,$$\n  with $q=0$.\n  $$\n  Q=0\n  $$"]["Intro"] =!=
    {{"Bullet", "Defined by"}, {"Math", "T = S,"}, {"Para", "with $q=0$."}, {"Math", "Q=0"}},
  Print["FAILED: parsing indented display equations"]; Exit[1]];

only = Rest[$ScriptCommandLine];
If[only === {} || AnyTrue[only, StringStartsQ["lab-"]],
  prep = RunProcess[{"python3", FileNameJoin[{$docDir, "manual", "reference.py"}]}];
  If[prep["ExitCode"] =!= 0, Print[prep["StandardError"]]; Exit[1]]];
pages = Flatten[Map[Function[kind, Map[<|"Name" -> FileBaseName[#], "File" -> #,
    "Cache" -> FileNameJoin[{$docDir, "cache", kind, FileBaseName[#] <> ".wl"}]|> &,
  FileNames["*.md", FileNameJoin[{$docDir, "src", kind}]]]], {"ref", "tutorial"}]];
pages = Join[pages, Map[<|"Name" -> FileBaseName[#], "File" -> #,
    "Cache" -> FileNameJoin[{$docDir, "manual", "cache", FileBaseName[#] <> ".wl"}]|> &,
  Join[FileNames["*.tex", FileNameJoin[{$docDir, "manual", "chapters"}]],
    FileNames["lab-*.tex", FileNameJoin[{$docDir, "manual", "generated", "labs"}]]]]];
If[Complement[only, Lookup[pages, "Name"]] =!= {},
  Print["Unknown documentation test: ", Complement[only, Lookup[pages, "Name"]]]; Exit[2]];
If[only =!= {}, pages = Select[pages, MemberQ[only, #["Name"]] &]];

tmp = CreateDirectory[];
(* Reference pages and chapters can share a basename (e.g. wick). Keep
   fresh evaluations separate, but retain the basename used by figure paths. *)
tempCache[p_] := FileNameJoin[{tmp, If[FileExtension[p["File"]] === "tex", "manual", "help"], p["Name"] <> ".wl"}];
jobs = Map[Append[#, "Cache" -> tempCache[#]] &, pages];
ok = SnegDoc`RunExamples[jobs];

failures = 0;
fail[name_, msg__] := (failures++; Print["FAILED: ", name, ": ", msg]);

Scan[Function[p, Module[{name = p["Name"], new, old, blocks},
  new = tempCache[p];
  blocks = If[FileExtension[p["File"]] === "tex",
    SnegDoc`TeXExampleBlocks[Import[p["File"], "Text", CharacterEncoding -> "UTF-8"]],
    SnegDoc`ExampleBlocks[Catch[SnegDoc`ParseDocFile[p["File"]], SnegDoc`DocError]]];
  Which[
    !FileExistsQ[new], fail[name, "examples were not evaluated"],
    !FileExistsQ[p["Cache"]], fail[name, "no cached outputs; run doc/build-docs.wl"],
    True,
      old = Get[p["Cache"]]["Results"];
      new = Get[new]["Results"];
      If[(Lookup[#, {"Code", "Reset"}] & /@ old) =!= blocks,
        fail[name, "the cache does not belong to the current source; rebuild it"]];
      If[Length[old] =!= Length[new], fail[name, "different number of example blocks"],
        MapThread[Function[{o, n},
          If[o =!= n, fail[name, "output differs for the example\n", n["Code"],
            "\ncached: ", Short[InputForm[o["Cells"]], 5], "\nnow:    ", Short[InputForm[n["Cells"]], 5]]]],
          {old, new}]]]]],
  pages];

Quiet[DeleteDirectory[tmp, DeleteContents -> True]];

Print["Pages: ", Length[pages], " Failures: ", failures];
If[!ok || failures > 0, Print["### FAILED ###"]; Exit[1]];
Print["### PASSED! ###"];
