<|"Source" -> "spectralmoments.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nsnegrealconstants[e, \
U];\nh = e number[d[]] + U hubbard[d[]];\nmom = spectralmoments[d[AN, UP], \
d[CR, UP], h, 2] /. avg[number[d[], DO]] -> w", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"1", ",", RowBox[{"e", "+", 
             RowBox[{"U", " ", "w"}]}], ",", RowBox[
            {SuperscriptBox["e", "2"], "+", RowBox[{"2", " ", "e", " ", "U", 
               " ", "w"}], "+", RowBox[{SuperscriptBox["U", "2"], " ", 
               "w"}]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "Factor[mom[[3]] - mom[[2]]^2]", "Reset" -> False, 
    "InLabel" -> "In[5]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"-", SuperscriptBox["U", "2"]}], " ", 
         RowBox[{"(", RowBox[{RowBox[{"-", "1"}], "+", "w"}], ")"}], " ", 
         "w"}], "Out[5]="}}|>, <|"Code" -> "snegfermionoperators[c];\nh = \
-hop[c[1], c[2]] + 2 Total[hubbard[{c[1], c[2]}]];\nmodes = {c[1], \
c[2]};\npoles = lehmann[c[AN, 1, UP], c[CR, 2, UP], h, modes, 1/2];\nmom = \
spectralmoments[c[AN, 1, UP], c[CR, 2, UP], h, 3, \"Average\" -> \
(thermalaverage[#, h, modes, 1/2] &)];\nMax[Abs[mom - Table[Total[(Last[#] \
If[k == 0, 1, First[#]^k]) & /@ poles], {k, 0, 3}]]] < 10^-9", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[6]="}}|>}|>
