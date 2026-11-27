repoRoot = DirectoryName[ExpandFileName[$InputFileName]];
buildDirectory = FileNameJoin[{repoRoot, "build"}];

fail[message_] := (
  Print["ERROR: ", message];
  Exit[1]
);

paclet = Get[FileNameJoin[{repoRoot, "PacletInfo.wl"}]];
pacletVersion = paclet["Version"];
packageVersions = StringCases[
  Import[FileNameJoin[{repoRoot, "sneg.m"}], "Text"],
  RegularExpression[
    "snegidstring\\s*=\\s*\"sneg\\.m\\s+([0-9]+(?:\\.[0-9]+)+)"
  ] -> "$1"
];

If[Length[packageVersions] =!= 1,
  fail["Could not determine the package version from sneg.m"]
];
packageVersion = First[packageVersions];

If[pacletVersion =!= packageVersion,
  fail[
    "Paclet version " <> pacletVersion <>
    " does not match package version " <> packageVersion
  ]
];

If[DirectoryQ[buildDirectory],
  DeleteDirectory[buildDirectory, DeleteContents -> True]
];
CreateDirectory[buildDirectory];

(* Documentation source notebooks from doc/src (examples cached in doc/cache). *)
docs = RunProcess[{"wolframscript", "-file", FileNameJoin[{repoRoot, "doc", "build-docs.wl"}], "nobuild"}];
Print[StringTrim[docs["StandardOutput"]]];
If[docs["ExitCode"] =!= 0,
  Print[docs["StandardError"]];
  fail["Generating the documentation notebooks failed"]
];

(* Stage the paclet: package files and documentation sources. PacletBuild
   turns the documentation sources into help pages. *)
stageDirectory = FileNameJoin[{buildDirectory, "stage", "SNEG"}];
CreateDirectory[stageDirectory, CreateIntermediateDirectories -> True];
Scan[CopyFile[FileNameJoin[{repoRoot, #}], FileNameJoin[{stageDirectory, #}]] &,
  {"PacletInfo.wl", "sneg.m", "LICENSE", "README.md"}];
CreateDirectory[FileNameJoin[{stageDirectory, "doc"}]];
CopyFile[FileNameJoin[{repoRoot, "doc", "LICENSE.txt"}],
  FileNameJoin[{stageDirectory, "doc", "LICENSE.txt"}]];
CopyDirectory[FileNameJoin[{repoRoot, "Kernel"}], FileNameJoin[{stageDirectory, "Kernel"}]];
CopyDirectory[FileNameJoin[{buildDirectory, "docsrc", "SNEG", "Documentation"}],
  FileNameJoin[{stageDirectory, "Documentation"}]];

Needs["PacletTools`"];
result = PacletBuild[stageDirectory, buildDirectory];
If[!MatchQ[result, _Success],
  Print[result];
  fail["PacletBuild failed"]
];

archive = result["PacletArchive"];
(* A successful PacletBuild can still package an incomplete set of pages.
   Compare the archive itself, including its search index, to the sources. *)
members = Import[archive, {"ZIP", "FileNames"}];
If[!VectorQ[members, StringQ], fail["Cannot read the paclet archive"]];
prefix = "SNEG-" <> pacletVersion <> "/Documentation/English/";
expected = Flatten[Map[Function[k,
  (prefix <> Last[k] <> "/" <> FileBaseName[#] <> ".nb" &) /@
    FileNames["*.md", FileNameJoin[{repoRoot, "doc", "src", First[k]}]]],
  {{"ref", "ReferencePages/Symbols"}, {"guide", "Guides"}, {"tutorial", "Tutorials"}}]];
missing = Complement[expected, members];
If[missing =!= {}, fail["Missing help notebooks: " <> StringRiffle[missing, ", "]]];
If[!AnyTrue[members, StringStartsQ[#, prefix <> "SearchIndex/"] &],
  fail["Missing documentation search index"]];
If[!MemberQ[members, "SNEG-" <> pacletVersion <> "/doc/LICENSE.txt"],
  fail["Missing CC BY 4.0 documentation license"]];
Print["Verified ", Length[expected], " help notebooks in the archive."];
stableArchive = FileNameJoin[{buildDirectory, "SNEG.paclet"}];
CopyFile[archive, stableArchive, OverwriteTarget -> True];

Print["Built ", archive];
Print["Built ", stableArchive];
