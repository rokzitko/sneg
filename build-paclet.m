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
    "snegidstring\\s*=\\s*\"sneg\\.m\\s+([0-9]+\\.[0-9]+\\.[0-9]+)"
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

Needs["PacletTools`"];
result = PacletBuild[repoRoot, buildDirectory];
If[!MatchQ[result, _Success],
  Print[result];
  fail["PacletBuild failed"]
];

archive = result["PacletArchive"];
stableArchive = FileNameJoin[{buildDirectory, "SNEG.paclet"}];
CopyFile[archive, stableArchive, OverwriteTarget -> True];

Print["Built ", archive];
Print["Built ", stableArchive];
