(* Audit the actual notebook expressions, including text, math boxes,
   examples and link labels. Skip comments (e.g. the notebook outline).
   Run after doc/build-docs.wl, or supply another Documentation/English root. *)
doc = DirectoryName[$InputFileName];
root = If[Length[$ScriptCommandLine] > 1, Last[$ScriptCommandLine],
  FileNameJoin[{ParentDirectory[doc], "Documentation", "English"}]];
expected = Sort[Flatten[Map[Function[k,
  FileNameJoin[{root, Last[k], FileBaseName[#] <> ".nb"}] & /@
    FileNames["*.md", FileNameJoin[{doc, "src", First[k]}]]],
  {{"ref", "ReferencePages/Symbols"}, {"guide", "Guides"}, {"tutorial", "Tutorials"}}]]];
files = Sort[FileNames["*.nb", root, Infinity]];
If[files =!= expected,
  Print["Missing notebooks: ", Complement[expected, files]];
  Print["Unexpected notebooks: ", Complement[files, expected]]; Exit[1]];

(* These are literal Wolfram names used in this documentation, including
   in paclet URIs. A trailing math delimiter still counts as a finding. *)
names = RegularExpression["\\$(?:Failed|RecursionLimit|Assumptions|Version|SnegVersion)(?![A-Za-z0-9])"];
badQ[s_String] := StringContainsQ[s, "$"] && StringContainsQ[StringReplace[s, names -> ""], "$"];
bad = 0;
Do[
  stream = OpenRead[file];
  nb = FirstCase[ReadList[stream, Hold[Expression]], Hold[_Notebook]];
  Close[stream];
  If[MissingQ[nb], Print["Unreadable notebook: ", file]; bad++; Continue[]];
  hits = DeleteDuplicates[Cases[nb, s_String /; badQ[s], Infinity]];
  If[hits =!= {}, bad++; Print[file, ": ", InputForm[hits]]],
  {file, files}];
Print["Checked ", Length[files], " help notebooks; pages with findings: ", bad];
Exit[Boole[bad > 0]];
