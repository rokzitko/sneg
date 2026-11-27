<|"Source" -> "lab-twist.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinlessfermionoperators[f];\nsnegrealconstan\
ts[t, theta];\nH = clusterhop[-t, f, f, 1, {3}, \"Twist\" -> theta];\nh = \
Table[braketop[f[CR, i, 0], H, f[CR, j, 0]], {i, 0, 2}, {j, 0, \
2}];\nSimplify[Eigenvalues[h /. {t -> 1, theta -> Pi/2}] - Table[-2 Cos[(2 Pi \
m + Pi/2)/3], {m, 0, 2}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0"}], 
         "}"}], "Out[5]=", <|"Kind" -> "List", "Parts" -> 
         {"0", "0", "0"}|>}}|>}|>
