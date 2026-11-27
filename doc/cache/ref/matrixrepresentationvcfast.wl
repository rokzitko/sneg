<|"Source" -> "matrixrepresentationvcfast.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t, \
U];\nfock = nonebasisvc[{c[1], c[2]}][[1, 2]];\nH = -t hop[c[1], c[2]] + U \
Total[hubbard[{c[1], c[2]}]];\nm = matrixrepresentationvcfast[H, \
fock];\nDimensions[m]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"16", ",", "16"}], "}"}], 
       "Out[6]="}}|>, <|"Code" -> "m == matrixrepresentationvc[H, fock]", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", "True", "Out[7]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1]}];\nmatrixrepresent\
ationvcfast[c[CR, 1, UP], {vc[1, 0], vc[0, 1]}, {vc[0, 0], vc[1, 0], vc[0, \
1]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "0", ",", "0"}], "}"}], 
           ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0"}], "}"}]}], 
         "}"}], "Out[3]="}}|>, <|"Code" -> "matrixrepresentationvcfast[spinx[\
c[1]], {(vc[1, 0] + vc[0, 1])/Sqrt[2], (vc[1, 0] - vc[0, 1])/Sqrt[2]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{FractionBox["1", "2"], ",", "0"}], 
             "}"}], ",", RowBox[{"{", RowBox[{"0", ",", RowBox[
                {"-", FractionBox["1", "2"]}]}], "}"}]}], "}"}], 
       "Out[1]="}}|>}|>
