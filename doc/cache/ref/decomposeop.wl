<|"Source" -> "decomposeop.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\ndecomposeop[(d[CR, UP] \
+ I d[CR, DO])/Sqrt[2], basis[d[]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0", ",", FractionBox["1", 
            SqrtBox["2"]], ",", FractionBox["\[ImaginaryI]", SqrtBox["2"]], 
           ",", "0"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nv = 2 + nc[d[CR, UP], d[CR, \
DO]];\nb = basis[d[]];\nExpand[decomposeop[v, b] . b - v]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[4]="}}|>}|>
