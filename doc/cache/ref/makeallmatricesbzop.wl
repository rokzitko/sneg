<|"Source" -> "makeallmatricesbzop.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nmakeallmatricesbzop[c[C\
R, 1, DO], qsbasis[{c[1], c[2]}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", 
                   RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", FractionBox[
                        "1", "2"]}], "}"}]}], "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
                   ",", RowBox[{"{", RowBox[{FractionBox["1", SqrtBox["2"]], 
                       ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", 
                       "1"}], "}"}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                      {"1", ",", FractionBox["1", "2"]}], "}"}], ",", 
                   RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}]}], "}"}], ",", 
               RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", 
                   RowBox[{"{", RowBox[{"-", "1"}], "}"}]}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", 
                     RowBox[{"2", ",", "0"}], "}"}], ",", RowBox[{"{", 
                     RowBox[{"1", ",", FractionBox["1", "2"]}], "}"}]}], 
                 "}"}], ",", RowBox[{"{", RowBox[{"{", RowBox[{"1", ",", 
                     "0"}], "}"}], "}"}]}], "}"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nbz = qszbasis[{c[1], \
c[2]}];\nmakeallmatricesbzop[c[CR, 1, UP], bz] == makeallmatricesbzvc[c[CR, \
1, UP], bzop2bzvc[bz]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[3]="}}|>}|>
