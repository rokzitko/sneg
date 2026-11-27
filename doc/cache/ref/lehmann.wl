<|"Source" -> "lehmann.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nh = hubbard[d[]] - \
number[d[]]/2;\nlehmann[d[AN, UP], d[CR, UP], h, {d}, 1/3]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"-", "0.5`"}], ",", "0.5`"}], "}"}], 
           ",", RowBox[{"{", RowBox[{"0.5`", ",", "0.5`"}], "}"}]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nh = -hop[c[1], \
c[2]];\nl = lehmann[c[AN, 1, UP], c[CR, 2, UP], h, {c[1], c[2]}, 0];\n{l, \
Chop[Total[l[[All, 2]]]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "0.999999999999998`"}], ",", "0.5`"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"0.999999999999998`", ",", 
                   RowBox[{"-", "0.5`"}]}], "}"}]}], "}"}], ",", "0"}], 
         "}"}], "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[d];\nh = \
hubbard[d[]] - number[d[]]/2;\nlehmann[number[d[]], number[d[]], h, {d}, 1]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", "}"}], "Out[3]="}}|>}|>
