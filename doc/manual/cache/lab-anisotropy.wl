<|"Source" -> "lab-anisotropy.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinoperators[{T, 1}];\nsnegrealconstants[Dz, \
E2];\nH = Dz/3 (stevens[2, 0, T[1]] + 2) + E2 stevens[2, 2, \
T[1]];\nEigenvalues[spinclustermatrices[H, {T[1]}, \"Blocks\" -> False]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", 
           RowBox[{"Dz", "-", "E2"}], ",", RowBox[{"Dz", "+", "E2"}]}], 
         "}"}], "Out[4]=", <|"Kind" -> "List", "Parts" -> 
         {"0", "\\text{Dz}-\\text{E2}", "\\text{Dz}+\\text{E2}"}|>}}|>}|>
