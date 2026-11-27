<|"Source" -> "lab-thermal.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t, \
U];\nmats = makematricesbzvc[-t hop[c[1], c[2]] + U Total[hubbard[{c[1], \
c[2]}]], qsbasisvc[{c[1], c[2]}]];\nZ[beta_] := Total[(2 #[[1, 2]] + 1) \
Total[Exp[-beta Eigenvalues[#[[2]]]]] & /@ mats];\n{Z[0], Z[1] /. {t -> 1, U \
-> 4} // N}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"16", ",", 
           "12.601737235707743`"}], "}"}], "Out[5]=", 
       <|"Kind" -> "List", "Parts" -> {"16", "12.6017"}|>}}|>}|>
