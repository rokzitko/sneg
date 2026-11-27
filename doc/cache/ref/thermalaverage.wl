<|"Source" -> "thermalaverage.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nh = hubbard[d[]] - \
number[d[]]/2;\nthermalaverage[#, h, {d}, 0] & /@ {spinz[d[]], \
pow[spinz[d[]], 2], hubbard[d[]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "0.25`", ",", 
           "0"}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nh = 2 hubbard[d[]] - \
number[d[]];\nAbs[thermalaverage[hubbard[d[]], h, {d}, 1] - 1/(2 + 2 Exp[1])] \
< 10^-12", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nh = hubbard[d[]] - number[d[]]/2 - \
10^-6 spinz[d[]];\nthermalaverage[spinz[d[]], h, {d}, 0, \"Tolerance\" -> #] \
& /@ {10^-10, 10^-5}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0.5`", ",", "0"}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nmodes = {c[1], \
c[2]}; h = -hop[c[1], c[2]];\nx = nc[c[CR, 1, UP], c[AN, 2, \
UP]];\nAbs[thermalaverage[x, h, modes, 1/2] - gaussianaverage[x, h, modes, \
1/2]] < 10^-12", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[4]="}}|>}|>
