<|"Source" -> "braketvc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t];\n\
makebasis[{c[1], c[2]}];\nbraketvc[vc[0, 0, 1, 0], -t hop[c[1], c[2]], vc[1, \
0, 0, 0]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"-", "t"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\nbraketvc[{vc[1, \
0], vc[0, 1]}, d[CR, UP], vacuum[]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nbraketvc[vc[1, 0, 0, 1], nc[spinplus[c[1]], spinminus[c[2]]], vc[0, \
1, 1, 0]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "1", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\nAbs[{braketvc[vc\
[1, 0], d[CR, UP], vc[0, 0]], braketvc[vc[1, 1], d[CR, DO], vc[1, 0]]}]^2", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "1"}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nop = I nc[c[CR, 2, UP], c[AN, 1, UP]];\na = vc[0, 0, 1, 0]; b = \
vc[1, 0, 0, 0];\nConjugate[braketvc[a, op, b]] == braketvc[b, conj[op], a]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[5]="}}|>}|>
