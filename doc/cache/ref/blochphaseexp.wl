<|"Source" -> "blochphaseexp.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegmomentumindexes[{k}, \
Ns];\nExpToTrig[blochphaseexp[blochphase[k, 1] + blochphase[-k, 1]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"2", " ", RowBox[{"Cos", "[", "k", 
           "]"}]}], "Out[2]="}}|>, <|"Code" -> "snegmomentumindexes[{k}, \
Ns];\nsneglatticevectors[a1, a2];\nblochphaseexp[blochphase[k, a1] + \
blochphase[k, a2], {k -> {kx, ky}, a1 -> {1, 0}, a2 -> {1/2, Sqrt[3]/2}}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{SuperscriptBox["\[ExponentialE]", 
          RowBox[{"\[ImaginaryI]", " ", "kx"}]], "+", SuperscriptBox[
          "\[ExponentialE]", RowBox[{"\[ImaginaryI]", " ", 
            RowBox[{"(", RowBox[{FractionBox["kx", "2"], "+", FractionBox[
                 RowBox[{SqrtBox["3"], " ", "ky"}], "2"]}], ")"}]}]]}], 
       "Out[3]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nsnegrealconstants[t];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nsneglatticevectors[a1, a2];\nHg = -t \
sum[hop[f[R, \"A\"], f[R, \"B\"]] + hop[f[R, \"A\"], f[R - a1, \"B\"]] + \
hop[f[R, \"A\"], f[R - a2, \"B\"]], {R}];\nh = blochhamiltonian[fourier[Hg, f \
-> fk, R -> k], {fk[\"A\"], fk[\"B\"]}, k]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{"0", ",", RowBox[{RowBox[{"-", "t"}], "-", 
                 RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                      {"\[ImaginaryI]", RowBox[{"-", "k"}], "\[CenterDot]", 
                       "a1"}]], DisplayForm]}], "-", RowBox[{"t", " ", 
                   TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                       RowBox[{"-", "k"}], "\[CenterDot]", "a2"}]], 
                    DisplayForm]}]}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "t"}], "-", 
                 RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                      {"\[ImaginaryI]", "k", "\[CenterDot]", "a1"}]], 
                    DisplayForm]}], "-", RowBox[{"t", " ", TagBox[
                    SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                       "\[CenterDot]", "a2"}]], DisplayForm]}]}], ",", "0"}], 
             "}"}]}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "Simplify[blochphaseexp[h, {k -> {4 Pi/3, 0}, a1 -> {1, 0}, a2 \
-> {1/2, Sqrt[3]/2}}]]", "Reset" -> False, "InLabel" -> "In[8]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}], 
       "Out[8]="}}|>, <|"Code" -> "Simplify[ExpToTrig[Eigenvalues[blochphasee\
xp[h, {k -> {kx, 0}, a1 -> {1, 0}, a2 -> {1/2, Sqrt[3]/2}}]]], Element[kx, \
Reals]]", "Reset" -> False, "InLabel" -> "In[9]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{"-", "t"}], " ", 
             RowBox[{"(", RowBox[{"1", "+", RowBox[{"2", " ", RowBox[
                    {"Cos", "[", FractionBox["kx", "2"], "]"}]}]}], ")"}]}], 
           ",", RowBox[{"t", "+", RowBox[{"2", " ", "t", " ", RowBox[
                {"Cos", "[", FractionBox["kx", "2"], "]"}]}]}]}], "}"}], 
       "Out[9]="}}|>}|>
