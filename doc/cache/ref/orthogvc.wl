<|"Source" -> "orthogvc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\north\
ogvc[{vc[1, 0] + vc[0, 1], vc[1, 0]}, {vc[1, 0], vc[0, 1]}] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["{vc[0, 1]/Sqrt[2] + \
vc[1, 0]/Sqrt[2], -(vc[0, 1]/Sqrt[2]) + vc[1, 0]/Sqrt[2]}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{Sneg`vc[0, 1]/Sqrt[2] + Sneg`vc[1, 0]/Sqrt[2], 
          -(Sneg`vc[0, 1]/Sqrt[2]) + Sneg`vc[1, 0]/Sqrt[2]}], 
        Editable -> True, AutoDelete -> True], "Out[3]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\northogvc[{vc[1, \
0] + vc[0, 1], vc[1, 0], 2 vc[0, 1]}, {vc[0, 0], vc[1, 0], vc[0, 1], vc[1, \
1]}] // InputForm", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["{vc[0, 1]/Sqrt[2] + \
vc[1, 0]/Sqrt[2], -(vc[0, 1]/Sqrt[2]) + vc[1, 0]/Sqrt[2]}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{Sneg`vc[0, 1]/Sqrt[2] + Sneg`vc[1, 0]/Sqrt[2], 
          -(Sneg`vc[0, 1]/Sqrt[2]) + Sneg`vc[1, 0]/Sqrt[2]}], 
        Editable -> True, AutoDelete -> True], "Out[3]//InputForm="}}|>, 
   <|"Code" -> "snegrealconstants[a];\northogvc[{a vc[1, 0] + vc[0, 1]}, \
{vc[1, 0], vc[0, 1]}] // InputForm", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{vc[0, 1]/Sqrt[1 + a^2] + (a*vc[1, 0])/Sqrt[1 + a^2]}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{Sneg`vc[0, 1]/Sqrt[1 + a^2] + (a*Sneg`vc[1, 0])/
            Sqrt[1 + a^2]}], Editable -> True, AutoDelete -> True], 
       "Out[2]//InputForm="}}|>, <|"Code" -> "snegfermionoperators[c];\nbz = \
qszbasisvc[{c[1], c[2]}];\nstates = Last[First[Select[bz, First[#] == {0, 0} \
&]]];\nP = nc[projector1[c[1]], projector1[c[2]]];\northogvc[ap[P, states], \
states] // InputForm", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{vc[0, 1, 1, 0], vc[1, 0, 0, 1]}", ShowStringCharacters -> 
          True, NumberMarks -> True], InputForm[{Sneg`vc[0, 1, 1, 0], 
          Sneg`vc[1, 0, 0, 1]}], Editable -> True, AutoDelete -> True], 
       "Out[5]//InputForm="}}|>, <|"Code" -> "snegfermionoperators[d];\nmakeb\
asis[{d[]}];\no = orthogvc[{vc[1, 0] + 2 vc[0, 1] + vc[1, 1], vc[1, 0] - \
vc[1, 1]}, {vc[0, 0], vc[1, 0], vc[0, 1], vc[1, 1]}];\nOuter[scalarproductvc, \
o, o]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}]}], "}"}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}]\
;\northogvc[{vc[1, 0]}, {vc[0, 1]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", RowBox[{"{", "}"}], 
       "Out[3]="}}|>}|>
