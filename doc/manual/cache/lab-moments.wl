<|"Source" -> "lab-moments.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nh = -hop[c[1], c[2]] + \
2 Total[hubbard[{c[1], c[2]}]];\nmodes = {c[1], c[2]};\npoles = lehmann[c[AN, \
1, UP], c[CR, 2, UP], h, modes, 1/2];\nmom = spectralmoments[c[AN, 1, UP], \
c[CR, 2, UP], h, 3, \"Average\" -> (thermalaverage[#, h, modes, 1/2] \
&)];\nMax[Abs[mom - Table[Total[(Last[#] If[k == 0, 1, First[#]^k]) & /@ \
poles], {k, 0, 3}]]] < 10^-9", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[6]=", <|"Kind" -> "Single", 
        "Parts" -> {"\\text{True}"}|>}}|>}|>
