<|"Source" -> "keymerge.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "keymerge[{{1, {a, b}}, {2, {c}}, {1, {d}}}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", RowBox[{"{", 
                 RowBox[{"a", ",", "b", ",", "d"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{"2", ",", RowBox[{"{", "c", "}"}]}], "}"}]}], 
         "}"}], "Out[1]="}}|>, <|"Code" -> "snegfermionoperators[c];\nb1 = \
qszbasis[{c[1]}]; b2 = qszbasis[{c[2]}];\nmakebasis[{c[1], c[2]}];\npairs = \
Flatten[Outer[{First[#1] + First[#2], Flatten[outer[Last[#1], Last[#2]]]} &, \
b1, b2, 1], 1];\n{First[#], Length[Last[#]]} & /@ keymerge[pairs]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "2"}], ",", "0"}], "}"}], ",", "1"}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], 
                   ",", RowBox[{"-", FractionBox["1", "2"]}]}], "}"}], ",", 
               "2"}], "}"}], ",", RowBox[{"{", RowBox[{RowBox[
                {"{", RowBox[{RowBox[{"-", "1"}], ",", FractionBox["1", 
                    "2"]}], "}"}], ",", "2"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", "1"}]}], "}"}], ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
               ",", "4"}], "}"}], ",", RowBox[
            {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], ",", 
               "1"}], "}"}], ",", RowBox[{"{", RowBox[{RowBox[
                {"{", RowBox[{"1", ",", RowBox[{"-", FractionBox["1", 
                      "2"]}]}], "}"}], ",", "2"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", "2"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0"}], "}"}], 
               ",", "1"}], "}"}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "keymerge[{{1, {a, b}}, {2, {c}}, {1, {d}}}] === \
mergebasis[{{1, {a, b}}, {2, {c}}, {1, {d}}}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[1]="}}|>}|>
