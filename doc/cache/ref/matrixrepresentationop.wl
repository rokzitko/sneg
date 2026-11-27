<|"Source" -> "matrixrepresentationop.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t];\n\
matrixrepresentationop[-t hop[c[1], c[2]], {c[CR, 1, UP], c[CR, 2, UP]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[{"-", "t"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"-", "t"}], ",", "0"}], 
             "}"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmatrixrepresentationop[nc[c[CR, 1, \
UP], c[CR, 1, DO]], {nc[c[CR, 1, UP], c[CR, 1, DO]], nc[c[CR, 2, UP], c[CR, \
2, DO]]}, {1}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", 
           RowBox[{"{", "0", "}"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "matrixrepresentationop[spinketbraP[1], spinket[1]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", SqrtBox["2"], ",", 
               "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", 
               SqrtBox["2"]}], "}"}], ",", RowBox[
            {"{", RowBox[{"0", ",", "0", ",", "0"}], "}"}]}], "}"}], 
       "Out[1]="}}|>, <|"Code" -> "matrixrepresentationop[#, {ket[0], ket[1], \
ket[2], ket[3]}] & /@ {phononplus[3], phononnumber[3]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", 
                   ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", 
                   "0", ",", "0", ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{"0", ",", SqrtBox["2"], ",", "0", ",", "0"}], 
                 "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", 
                   SqrtBox["3"], ",", "0"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", 
                   ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", 
                   "1", ",", "0", ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{"0", ",", "0", ",", "2", ",", "0"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "3"}], 
                 "}"}]}], "}"}]}], "}"}], "Out[1]="}}|>, 
   <|"Code" -> "matrixrepresentationop[komutator[phononminus[3], \
phononplus[3]], {ket[0], ket[1], ket[2], ket[3]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{"1", ",", "0", ",", "0", ",", "0"}], "}"}], 
           ",", RowBox[{"{", RowBox[{"0", ",", "1", ",", "0", ",", "0"}], 
             "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", 
               "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", 
               "0", ",", RowBox[{"-", "3"}]}], "}"}]}], "}"}], "Out[1]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nbz = qszbasisvc[{c[1], \
c[2]}];\nstates = Flatten[bz[[All, 2]]];\nop = number[c[1]] + nc[c[CR, 1, \
UP], c[AN, 2, DO]];\nmatrixrepresentationop[op, vc2ops[states]] == \
matrixrepresentationvc[op, states]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmatrixrepresentationop[1, {nc[c[CR, \
1, UP], VACUUM], c[CR, 1, UP]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", RowBox[{"vevwick", "[", 
                 TagBox[StyleBox["\"\[LeftAngleBracket]\[Diameter]\
\[RightDoubleBracketingBar]\"", FontColor -> RGBColor[1, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 1]] & ], "]"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"vevwick", "[", 
                 TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\[Diameter]\
\[RightAngleBracket]\"", FontColor -> RGBColor[1, 0, 1]], StyleForm[#1, 
                    FontColor -> RGBColor[1, 0, 1]] & ], "]"}], ",", "1"}], 
             "}"}]}], "}"}], "Out[2]="}}|>}|>
