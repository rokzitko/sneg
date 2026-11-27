<|"Source" -> "lab-stoner.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "L = 6; T0 = 1/10; Ne = 4;\nek = Table[-2 Cos[2 Pi \
j/L], {j, 0, L - 1}];\nmu0 = mu /. FindRoot[2 Total[1/(Exp[(ek - mu)/T0] + \
1)] == Ne, {mu, -1}];\nUc = 1/(Total[1/(4 T0 Cosh[(ek - mu0)/(2 T0)]^2)]/L)", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "1.1998910624184778`", "Out[4]=", 
       <|"Kind" -> "Single", "Parts" -> {"1.19989"}|>}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nring[U_] := -Sum[hop[c[i], c[Mod[i, \
L] + 1]], {i, L}] + U Sum[hubbard[c[i]], {i, L}];\nmagnetization[U_] := \
Module[{s = meanfieldsolve[meanfield[ring[U], \"Hartree\"], Table[c[i], {i, \
L}], Flatten[Table[{avg[number[c[i], UP]] -> 0.5, avg[number[c[i], DO]] -> \
0.2}, {i, L}]], \"ParticleNumber\" -> Ne, \"Temperature\" -> T0]},\n  \
Chop[Sum[avg[number[c[i], UP]] - avg[number[c[i], DO]], {i, L}]/L /. \
s[\"Averages\"], 10^-6]];\n{magnetization[0.9 Uc], magnetization[1.3 Uc]}", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", 
           "0.2507766176803345`"}], "}"}], "Out[8]=", 
       <|"Kind" -> "List", "Parts" -> {"0", "0.250777"}|>}}|>}|>
