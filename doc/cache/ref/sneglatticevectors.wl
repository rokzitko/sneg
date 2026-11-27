<|"Source" -> "sneglatticevectors.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "sneglatticevectors[a1, a2];\n{Conjugate[a1], \
latticeindexQ[a1], listlatticevectors}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"a1", ",", "False", ",", 
           RowBox[{"{", RowBox[{"a1", ",", "a2"}], "}"}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nsnegrealconstants[t];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nsneglatticevectors[a1, a2];\nH = -t \
sum[hop[f[R], f[R + a1]] + hop[f[R], f[R + a2]], {R}];\nh = \
blochhamiltonian[fourier[H, f -> fk, R -> k], {fk}, k]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"{", RowBox[
            {RowBox[{RowBox[{"-", "t"}], " ", TagBox[SuperscriptBox["e", 
                 RowBox[{"\[ImaginaryI]", RowBox[{"-", "k"}], "\[CenterDot]", 
                   "a1"}]], DisplayForm]}], "-", RowBox[{"t", " ", TagBox[
                SuperscriptBox["e", RowBox[{"\[ImaginaryI]", RowBox[
                    {"-", "k"}], "\[CenterDot]", "a2"}]], DisplayForm]}], 
             "-", RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                  {"\[ImaginaryI]", "k", "\[CenterDot]", "a1"}]], 
                DisplayForm]}], "-", RowBox[{"t", " ", TagBox[SuperscriptBox[
                 "e", RowBox[{"\[ImaginaryI]", "k", "\[CenterDot]", "a2"}]], 
                DisplayForm]}]}], "}"}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "Simplify[ExpToTrig[blochphaseexp[h, {k -> {kx, ky}, a1 -> {1, \
0}, a2 -> {0, 1}}]]]", "Reset" -> False, "InLabel" -> "In[8]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{"{", RowBox[{RowBox[{"-", "2"}], " ", "t", " ", 
             RowBox[{"(", RowBox[{RowBox[{"Cos", "[", "kx", "]"}], "+", 
                 RowBox[{"Cos", "[", "ky", "]"}]}], ")"}]}], "}"}], "}"}], 
       "Out[8]="}}|>, <|"Code" -> "sneglatticeindexes[{R}, \
Ns];\nsneglatticevectors[a1];\nsumSimplifyKD[KroneckerDelta[R + a1, R]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TemplateBox[
        {RowBox[{"0", ",", RowBox[{"-", "a1"}]}]}, "KroneckerDeltaSeq"], 
       "Out[3]="}}|>}|>
