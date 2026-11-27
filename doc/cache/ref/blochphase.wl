<|"Source" -> "blochphase.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegmomentumindexes[{k, q}, \
Ns];\nsneglatticeindexes[{R}, Ns];\nblochphase[k, R]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
           "\[CenterDot]", "R"}]], DisplayForm], "Out[3]="}}|>, 
   <|"Code" -> "blochphase[k, R] blochphase[q, R]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
           RowBox[{"k", "+", "q"}], "\[CenterDot]", "R"}]], DisplayForm], 
       "Out[4]="}}|>, <|"Code" -> "snegmomentumindexes[{k}, \
Ns];\nsneglatticeindexes[{R}, Ns];\nsneglatticevectors[a1];\n{blochphase[k, R \
+ a1], blochphase[k, R + 2], blochphase[k, 2 R]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                 "\[CenterDot]", "a1"}]], DisplayForm], " ", 
             TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                 "\[CenterDot]", "R"}]], DisplayForm]}], ",", 
           RowBox[{TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                 "\[CenterDot]", "R"}]], DisplayForm], " ", 
             TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                 RowBox[{"2", " ", "k"}], "\[CenterDot]", "1"}]], 
              DisplayForm]}], ",", TagBox[SuperscriptBox["e", 
             RowBox[{"\[ImaginaryI]", RowBox[{"2", " ", "k"}], 
               "\[CenterDot]", "R"}]], DisplayForm]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegmomentumindexes[{k}, \
Ns];\nsneglatticevectors[a1];\n{Conjugate[blochphase[k, a1]], \
conj[blochphase[k, a1]], blochphase[k, a1]^3, blochphase[0, a1]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
               RowBox[{"-", "k"}], "\[CenterDot]", "a1"}]], DisplayForm], 
           ",", TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", RowBox[
                {"-", "k"}], "\[CenterDot]", "a1"}]], DisplayForm], ",", 
           TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", RowBox[
                {"3", " ", "k"}], "\[CenterDot]", "a1"}]], DisplayForm], ",", 
           "1"}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "blochphase[{kx, ky}, {1, 0}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", SuperscriptBox["\[ExponentialE]", 
        RowBox[{"\[ImaginaryI]", " ", "kx"}]], "Out[1]="}}|>, 
   <|"Code" -> "snegmomentumindexes[{k, p}, Ns];\nsneglatticeindexes[{R, Rp}, \
Ns];\n{sumSimplifyKD[sum[blochphase[k - p, R], {R}]], \
sumSimplifyKD[sum[blochphase[k, R - Rp], {k}]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{"Ns", " ", TemplateBox[
              {RowBox[{"k", ",", "p"}]}, "KroneckerDeltaSeq"]}], ",", 
           RowBox[{"Ns", " ", TemplateBox[{RowBox[{"R", ",", "Rp"}]}, 
              "KroneckerDeltaSeq"]}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegmomentumindexes[{k, p}, Ns];\nsneglatticeindexes[{R}, \
Ns];\nsneglatticevectors[a1];\nsnegcomplexfunctions[g];\nsumSimplifyKD[sum[bl\
ochphase[k, R + a1] blochphase[-p, R] g[k], {R}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"Ns", " ", TagBox[SuperscriptBox["e", 
           RowBox[{"\[ImaginaryI]", "k", "\[CenterDot]", "a1"}]], 
          DisplayForm], " ", RowBox[{"g", "[", "k", "]"}], " ", 
         TemplateBox[{RowBox[{"k", ",", "p"}]}, "KroneckerDeltaSeq"]}], 
       "Out[5]="}}|>, <|"Code" -> "snegmomentumindexes[{k}, \
Ns];\nExpToTrig[blochphaseexp[blochphase[k, 1] + blochphase[-k, 1]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"2", " ", RowBox[{"Cos", "[", "k", 
           "]"}]}], "Out[2]="}}|>, <|"Code" -> "snegmomentumindexes[{k}, \
Ns];\nsneglatticeindexes[{R}, Ns];\nsumSimplifyKD[sum[Cos[blochphase[k, R]], \
{R}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{"Cos", "[", TagBox[SuperscriptBox["e", RowBox[
                {"\[ImaginaryI]", "k", "\[CenterDot]", "R"}]], DisplayForm], 
             "]"}], ",", RowBox[{"{", "R", "}"}]}], "]"}], "Out[3]="}}|>}|>
