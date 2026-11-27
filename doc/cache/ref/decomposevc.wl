<|"Source" -> "decomposevc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\ndeco\
mposevc[(vc[1, 0] + I vc[0, 1])/Sqrt[2], {vc[0, 0], vc[1, 0], vc[0, 1], vc[1, \
1]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", FractionBox["1", 
            SqrtBox["2"]], ",", FractionBox["\[ImaginaryI]", SqrtBox["2"]], 
           ",", "0"}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nbz = qszbasisvc[{c[1], \
c[2]}];\nstates = Last[First[Select[bz, First[#] == {0, 0} &]]];\nsinglet = \
(vc[1, 0, 0, 1] - vc[0, 1, 1, 0])/Sqrt[2];\ncoef = decomposevc[singlet, \
states]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", 
           RowBox[{"-", FractionBox["1", SqrtBox["2"]]}], ",", 
           FractionBox["1", SqrtBox["2"]], ",", "0"}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "Expand[coef . states - singlet]", "Reset" -> False, 
    "InLabel" -> "In[6]:=", "Cells" -> {{"Output", "0", "Out[6]="}}|>}|>
