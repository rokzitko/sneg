<|"Source" -> "removestates.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\nremo\
vestates[{vc[0, 0], vc[1, 0], vc[0, 1]}, {(vc[1, 0] + vc[0, 1])/Sqrt[2], \
vc[0, 0]}] // InputForm", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{-(vc[0, 1]/Sqrt[2]) + vc[1, 0]/Sqrt[2]}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{-(Sneg`vc[0, 1]/Sqrt[2]) + Sneg`vc[1, 0]/Sqrt[2]}], 
        Editable -> True, AutoDelete -> True], "Out[3]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nbz = qszbasisvc[{c[1], \
c[2]}];\nstates = Last[First[Select[bz, First[#] == {0, 0} &]]];\nv = \
ap[spinminus[c[1]] + spinminus[c[2]], vc[1, 0, 1, 0]];\nremovestates[states, \
{v/normvc[v]}] // InputForm", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["{vc[0, 0, 1, 1], vc[0, \
1, 1, 0]/Sqrt[2] - vc[1, 0, 0, 1]/Sqrt[2], vc[1, 1, 0, 0]}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{Sneg`vc[0, 0, 1, 1], Sneg`vc[0, 1, 1, 0]/Sqrt[2] - 
           Sneg`vc[1, 0, 0, 1]/Sqrt[2], Sneg`vc[1, 1, 0, 0]}], 
        Editable -> True, AutoDelete -> True], "Out[5]//InputForm="}}|>}|>
