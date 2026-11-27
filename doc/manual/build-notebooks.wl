(* Export the manual case studies as ordinary, self-contained Mathematica notebooks. *)
dir = DirectoryName[$InputFileName];
Get[FileNameJoin[{ParentDirectory[dir], "tools", "SnegDoc.wl"}]];
prep = RunProcess[{"python3", FileNameJoin[{dir, "reference.py"}]}];
If[prep["ExitCode"] =!= 0, Print[prep["StandardError"]]; Exit[1]];
studies = Import[FileNameJoin[{dir, "generated", "studies.json"}], "RawJSON"];
dest = FileNameJoin[{ParentDirectory[dir], "notebooks"}];
If[!DirectoryQ[dest], CreateDirectory[dest]];
topics = {
  {"exchange", "Direct and exchange contractions", "Chapter 6"},
  {"ssh", "The SSH chain", "Chapter 8"}, {"graphene", "The Dirac point of graphene", "Chapter 8"},
  {"rashba", "Spin splitting in the Rashba chain", "Chapter 8"},
  {"holstein", "Momentum transfer in the Holstein model", "Chapter 9"},
  {"lang-firsov", "The displaced impurity oscillator", "Chapter 9"},
  {"gap", "The finite-temperature BCS gap equation", "Chapter 11"},
  {"andreev", "Singlet and doublet in a superconducting dot", "Chapter 11"},
  {"kitaev", "Gap closings of the Kitaev chain", "Chapter 11"},
  {"magnons", "Spin waves in a staggered field", "Chapter 12"},
  {"xxz", "The Jordan-Wigner transformation of the XXZ chain", "Chapter 12"},
  {"anderson", "From the Anderson model to exchange", "Chapter 13"},
  {"lehmann", "Bonding and antibonding spectral weight", "Chapter 14"}};
write[key_, title_, chapter_, body_, results_] := Module[{doc, nb},
  doc = SnegDoc`ParseDocString["# " <> title <> "\n\n" <> body];
  nb = Catch[Check[SnegDoc`StudyNotebook[doc, results, chapter], $Failed], SnegDoc`DocError];
  If[!MatchQ[nb, _Notebook], Print["Failed: ", key, ": ", nb]; Exit[1]];
  Block[{$CharacterEncoding = "ASCII", $ContextPath = {"System`"}, $Context = "Global`"},
    Put[nb, FileNameJoin[{dest, key <> ".nb"}]]]];
UsingFrontEnd[Scan[Function[row, With[{key = First[row]},
  write[key, row[[2]], row[[3]], studies[key],
    Get[FileNameJoin[{dir, "cache", "lab-" <> key <> ".wl"}]]["Results"]]]], topics]];

(* Select by a recognizable assignment rather than hard-coded cell numbers.
   All prerequisites of a selected calculation are explicit in its sequence. *)
blocks = First /@ SnegDoc`TeXExampleBlocks[Import[FileNameJoin[{dir,"chapters","correlated-electrons.tex"}],
  "Text", CharacterEncoding -> "UTF-8"]];
results = Get[FileNameJoin[{dir,"cache","correlated-electrons.wl"}]]["Results"];
If[Lookup[results,"Code"] =!= blocks, Print["Rebuild the correlated-electrons cache."]; Exit[1]];
chapterStudy[key_, title_, selectors_] := Module[{chosen, body, ids},
  ids = Map[Function[p, Select[Range[Length[blocks]], StringStartsQ[blocks[[#]], First[p]] &]], selectors];
  If[!AllTrue[ids, Length[#] == 1 &], Print["Missing or ambiguous chapter example in ", key]; Exit[1]];
  chosen = results[[Flatten[ids]]];
  body = StringRiffle[MapThread[#1[[2]] <> "\n```wl\n" <> #2["Code"] <> "\n```" &, {selectors,chosen}], "\n\n"];
  UsingFrontEnd[write[key, title, "Chapter 10", body, chosen]]];
chapterStudy["hubbard-dimer","Superexchange in the Hubbard dimer", {
  {"Needs[", "Declare the electron operators and real parameters. Energies are measured relative to an empty site."},
  {"sites =", "Connect two sites by a single bond and check both charge and spin conservation. The calculation reformulates Steeb and Hardy, Bose, Spin and Fermi Systems: Problems and Solutions (2015), section 3.4, problem 84, in a total-spin basis."},
  {"bd =", "At half filling select the charge-zero subspace and separate singlet and triplet."},
  {"hs //", "The two ionic states couple to the covalent singlet but not to the triplet."},
  {"Factor[CharacteristicPolynomial", "Factor the singlet characteristic polynomial. One doublon state remains at energy $U$."},
  {"emin =", "Half the derivative of the lowest energy with respect to $U$ gives the double occupation per site."},
  {"p = nc @@", "Restrict to single occupancy and eliminate virtual charge states through second order in hopping."},
  {"Normal[Series[-emin", "Compare with the strong-coupling expansion of the exact singlet-triplet gap for $U>0$."}}];
chapterStudy["atomic-propagator","Spectral poles of the Hubbard atom", {
  {"Needs[", "Build the atomic Hamiltonian with a level energy measured relative to the chemical potential."},
  {"gatom =", "The opposite-spin occupation $w$ assigns weights to the two addition energies. Keep it symbolic."},
  {"ph = Factor", "At particle-hole symmetry the two poles have equal weight. Check the high-frequency normalization."},
  {"asym = atom", "Move away from symmetry and calculate the transition energies and thermal weights independently by a Lehmann sum."},
  {"wth = thermalaverage", "At a complex test frequency compare the Lehmann result with the equation of motion using the exact thermal occupation."}}];
Print["Wrote ", Length[topics] + 2, " notebooks to ", dest];
