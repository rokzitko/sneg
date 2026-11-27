<|"Source" -> "expvvc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nsinglet = (vc[1, 0, 0, 1] - vc[0, 1, 1, \
0])/Sqrt[2];\nexpvvc[spinspin[c[1], c[2]], singlet]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"-", FractionBox["3", "4"]}], "Out[4]="}}|>, 
   <|"Code" -> "expvvc[spinspin[c[1], c[2]], vc[1, 0, 1, 0]]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", FractionBox["1", "4"], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], c[2]}];\npsi = \
(vc[1, 1, 0, 0] + vc[0, 0, 1, 1])/Sqrt[2];\nexpvvc[{number[c[1]], \
hubbard[c[1]], nc[number[c[1]], number[c[2]]]}, psi]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"1", ",", FractionBox["1", "2"], ",", 
           "0"}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nbz = qsbasisvc[{c[1], \
c[2]}];\nstates = Last[First[Select[bz, First[#] == {0, 0} &]]];\nH = \
-hop[c[1], c[2]] + 4 Total[hubbard[{c[1], c[2]}]];\n{vals, vecs} = \
Eigensystem[matrixrepresentationvc[H, states]];\npsi = \
Normalize[vecs[[First[Ordering[N[vals]]]]]] . states;\nSimplify[expvvc[{H, \
hubbard[c[1]]}, psi]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"2", "-", RowBox[{"2", " ", SqrtBox["2"]}]}], 
           ",", FractionBox["1", RowBox[{"8", "+", RowBox[{"4", " ", 
                SqrtBox["2"]}]}]]}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "N[{1/(8 + 4 Sqrt[2]), (1 - 4/Sqrt[32])/4}]", 
    "Reset" -> False, "InLabel" -> "In[8]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0.07322330470336312`", ",", 
           "0.07322330470336313`"}], "}"}], "Out[8]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\na = (vc[1, 0] + \
vc[1, 1])/Sqrt[2];\nexpvvc[number[d[]], a] == braketvc[a, number[d[]], a]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\nexpvvc[number[d[\
]], 2 vc[1, 0]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "4", "Out[3]="}}|>}|>
