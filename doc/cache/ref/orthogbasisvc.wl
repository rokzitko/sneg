<|"Source" -> "orthogbasisvc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nbz = \
qszbasisvc[{d[]}];\nbz1 = applybasis[bz, 2 # &];\northogbasisvc[bz1, bz] // \
InputForm", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["{{{-1, 0}, {vc[0, \
0]}}, {{0, -1/2}, {vc[0, 1]}}, {{0, 1/2}, {vc[1, 0]}}, {{1, 0}, {vc[1, 1]}}}"\
, ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{{{-1, 0}, {Sneg`vc[0, 0]}}, {{0, -1/2}, {Sneg`vc[0, 1]}}, 
          {{0, 1/2}, {Sneg`vc[1, 0]}}, {{1, 0}, {Sneg`vc[1, 1]}}}], 
        Editable -> True, AutoDelete -> True], "Out[4]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nbz = qszbasisvc[{d[]}];\nbz1 = \
applybasis[bz, ap[projector02[d[]], #] \
&];\ndropemptysubspaces[orthogbasisvc[bz1, bz]] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{{{-1, 0}, {vc[0, 0]}}, {{1, 0}, {vc[1, 1]}}}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{{{-1, 0}, {Sneg`vc[0, 0]}}, {{1, 0}, {Sneg`vc[1, 1]}}}], 
        Editable -> True, AutoDelete -> True], "Out[4]//InputForm="}}|>}|>
