<|"Source" -> "expvop.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\ns = (nc[c[CR, 1, UP], \
c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]])/Sqrt[2];\nt0 = (nc[c[CR, 1, \
UP], c[CR, 2, DO]] + nc[c[CR, 1, DO], c[CR, 2, UP]])/Sqrt[2];\nstot = \
spinss[c[1]] + spinss[c[2]] + 2 spinspin[c[1], c[2]];\n{expvop[stot, s], \
expvop[stot, t0]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "2"}], "}"}], 
       "Out[5]="}}|>, <|"Code" -> "snegfermionoperators[c];\nexpvop[{number[c\
[1]], number[c[2]], spinz[c[1]]}, c[CR, 1, UP]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"1", ",", "0", ",", FractionBox["1", 
            "2"]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\npsi = c[CR, 1, UP] + c[CR, 2, \
UP];\n{expvop[number[c[1]], psi], expvop[number[c[1]], \
psi]/scalarproductop[psi, psi]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", FractionBox["1", 
            "2"]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[f];\nordering[f] = \
SEA;\n{expvop[number[f[-1]], 1], expvop[number[f[-1]], f[AN, -1, UP]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"2", ",", "1"}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrealconstant\
s[t, U];\nH = -t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]);\ns = \
(nc[c[CR, 1, UP], c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]])/Sqrt[2];\nd \
= (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[CR, 2, UP], c[CR, 2, \
DO]])/Sqrt[2];\n{expvop[H, s], expvop[H, d], braketop[d, H, s]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "U", ",", 
           RowBox[{RowBox[{"-", "2"}], " ", "t"}]}], "}"}], "Out[6]="}}|>}|>
